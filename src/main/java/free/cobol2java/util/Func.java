package free.cobol2java.util;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class Func {
    private Map<String,String> fieldToType = new HashMap<>();
    private Map<String,String> fieldToClassType = new HashMap<>();
    private Map<String,String> fieldToQualifiedName = new HashMap<>();
    private Map<String,String> qualifiedNameToDimLevel = new HashMap<>();
    private Stack<String> clsLevel = new Stack<>();
    private Stack<Integer> curDims = new Stack<>();
    public int dim_push(Number dim){
        return curDims.push(dim.intValue());
    }
    public int dim_pop(){
        return curDims.pop();
    }

    public int dim_peek(){
        return curDims.peek();
    }
    public int dim_size(){
        return curDims.size();
    }

    public String dim_putQlfLevel(String qlfName, String dimLevelStr){
        if(qlfName == null || "null".equals(qlfName))
            return null;
        qualifiedNameToDimLevel.put(qlfName,dimLevelStr);
        String realQlfName = dimLevelStr.lastIndexOf(",0") != -1 ? qlfName.substring(0, qlfName.lastIndexOf('.')) : qlfName;
        return qualifiedNameToDimLevel.put(realQlfName,dimLevelStr);
    }

    public String dim_getQlfLevel(String qlfName){
        return qualifiedNameToDimLevel.get(qlfName);
    }
    public String dim_value(){
        String ret = "";
        for(int i = 0;i<curDims.size();i++){
            if(i !=0)
                ret += ",";
            ret += curDims.get(i);
        }
        return ret;
    }
    public String name_toClass(String cblName){
        String[] parts = cblName.split("-");
        String ret = "";
        for(int i = 0;i<parts.length;i++){
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }
        return ret;
    }
    public String name_toField(String cblName){
        String[] parts = cblName.split("-");
        String ret = parts[0].toLowerCase();
        for(int i = 1;i<parts.length;i++){
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }

        return ret;
    }

    public String name_getFieldType(String fieldName){
        String ret = fieldToType.get(fieldName);
        return ret == null ? name_getFieldClsType(fieldName) : ret;
    }
    public String name_getFieldClsType(String fieldName){
        return fieldToClassType.get(fieldName);
    }
    public String name_setFieldType(String fieldName, String type){
        return fieldToType.put(fieldName, type);
    }
    public String name_setFieldClsType(String fieldName, String type){
        return fieldToClassType.put(fieldName, type);
    }
    public String type_getType(String cblType){
        boolean bString=cblType.indexOf("X(") != -1|| cblType.indexOf("A(") != -1;
        if(bString)
            return "String";
        boolean bFloat = cblType.indexOf("V") != -1;
        if(bFloat)
            return "Double";
        return "Integer";
    }

    public String name_enterClass(String fieldName){
        return clsLevel.push(fieldName);
    }
    public String name_exitClass(){
        return clsLevel.pop();
    }
    public String name_putInnerField(String fieldName){
        String qualifiedName = clsLevel.size() == 0 ? null : createQualifedName(fieldName);
        if(qualifiedName != null)
            fieldToQualifiedName.put(fieldName,qualifiedName);
        return qualifiedName;
    }
    public String name_delegateName(String fieldName) {
        String ret = fieldToQualifiedName.get(fieldName);
        if( ret == null)
            ret = fieldName;
        return name_delegateName1(ret,null);
    }
    public String name_delegateName1(String fieldName, String dimStr){
        String ret = fieldName;
        if(dimStr != null) {
            String dimLevelStr = dim_getQlfLevel(fieldName);
            String[] dims = dimStr.split(",");
            String[] names = ret.split("\\.");
            String[] level = dimLevelStr.split(",");
            if(level.length == names.length){
                String[] lv = new String[names.length-1];
                System.arraycopy(level,0,lv,0,lv.length);
                level = lv;
            }
            ret = "";
            int usedDim = 0;
            for(int i = 0;i<level.length;i++){
                String node;
                if(level[level.length-1-i].equals("0")) {
                    node = names[names.length-1-i];
                }else{
                    node = names[names.length-1-i] +"["+dims[dims.length-1-usedDim]+"]";
                    usedDim++;
                }
                if(i != 0){
                    node += ".";
                }
                ret = node +ret;
            }
            for(int i= level.length;i<names.length;i++){
                ret = names[names.length-i-1]+"." +ret;
            }
        }
        return ret;
    }
    private String createQualifedName(String fieldName) {
        String ret = "";
        for(String superField:clsLevel){
            ret +=superField+".";
        }
        return ret + fieldName;
    }

    public String getQualifedName(String fieldName){
        return fieldToQualifiedName.get(fieldName);
    }

    public String expr_convertExpr(String cobolExpr){
        String expression = cobolExpr.replace("**","^");
        // Regular expression to match variables of the form identifier(-identifier)*
        Pattern varPattern = Pattern.compile("\\b[a-zA-Z_][a-zA-Z0-9_]*(?:-[a-zA-Z_][a-zA-Z0-9_]*)*\\b");
        Matcher varMatcher = varPattern.matcher(expression);

        // Extract all variables
        List<String> variables = new ArrayList<>();
        while (varMatcher.find()) {
            variables.add(varMatcher.group());
        }
        String ret = expression;
        for(String id:variables){
            String fieldName = name_toField(id);
            String[] dims = getDimStringOfVar(cobolExpr,id);
            String dimStr = dims[1];
            if(dimStr == null) {
                ret = ret.replace(id, name_delegateName(fieldName));
            }else{
                String qlfNameWithDims = name_delegateName1(name_delegateName(fieldName),dimStr);
                ret = dims[0] + qlfNameWithDims+dims[2];
            }
        }
        return ret.indexOf('^') != -1 ? ExprUtil.convertExpression(ret):ret;
    }
    private String[] getDimStringOfVar(String cobolExpr, String var){
        String [] ret = new String[3];
        int index = cobolExpr.indexOf(var);
        String sub = cobolExpr.substring(index + var.length()).trim();
        ret[0] = cobolExpr.substring(0,index);
        ret[2] = sub;
        if(sub.length() !=0 && sub.charAt(0) == '('){
            int leftIndex = sub.indexOf(')');
            String dimString = sub.substring(1,leftIndex);
            ret[1] = dimString;
            ret[2] = sub.substring(leftIndex+1);
        }
        return ret;
    }

    public String array_initString(String dims, String val) {
        String[] vals = dims.split(",");
        String str = val;
        for (int i = vals.length - 1; i >= 0; i--) {
            String arr = _initString(vals[i],str);
            str = arr;
        }
        return str;
    }
    private String _initString(String num, String str) {
        int n = Integer.parseInt(num);
        // Check for invalid input
        if (n <= 0 || str == null) {
            return "{}";
        }

        // Use a StringBuilder to construct the result efficiently
        StringBuilder result = new StringBuilder("{");

        // Append the string `str` `n` times, separated by commas
        for (int i = 0; i < n; i++) {
            result.append(str);
            if (i < n - 1) {
                result.append(",");
            }
        }

        // Close the curly braces
        result.append("}");

        // Convert StringBuilder to String and return
        return result.toString();
    }
    private String capitalizeFirstLetter(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

}
