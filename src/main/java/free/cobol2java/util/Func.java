package free.cobol2java.util;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class Func {
    private Map<String,String> fieldToQualifiedName = new HashMap<>();
    private Stack<String> clsLevel = new Stack<>();
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

    public String name_delegateName(String fieldName){
        String ret = fieldToQualifiedName.get(fieldName);
        if(ret == null)
            ret = fieldName;
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
            ret = ret.replace(id,name_delegateName(name_toField(id)));
        }
        return ret.indexOf('^') != -1 ? ExprUtil.convertExpression(ret):ret;
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
