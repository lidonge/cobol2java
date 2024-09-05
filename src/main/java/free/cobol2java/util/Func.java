package free.cobol2java.util;

import io.proleap.cobol.CobolParser;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class Func {
    private Map<String, String> fieldToType = new HashMap<>();
    private Map<String, String> fieldToClassType = new HashMap<>();
    private Map<String, String> fieldToQualifiedName = new HashMap<>();
    private Map<String, String> dimFieldToQualifiedName = new HashMap<>();
    private Map<String, String> qualifiedNameToDimLevel = new HashMap<>();
    private Stack<String> clsLevel = new Stack<>();
    private Stack<Integer> curDims = new Stack<>();

    public int dim_push(Number dim) {
        return curDims.push(dim.intValue());
    }

    public int dim_pop() {
        return curDims.pop();
    }

    public int dim_peek() {
        return curDims.peek();
    }

    public int dim_size() {
        return curDims.size();
    }

    public String dim_putQlfLevel(String fieldName, String dimLevelStr) {
        String qlfName = fieldToQualifiedName.get(fieldName);
        if (qlfName == null || "null".equals(qlfName)) {
            String[] dims = dimLevelStr.split(",");
            if (dims[dims.length - 1].equals("0"))
                return null;
        }
        qualifiedNameToDimLevel.put(qlfName, dimLevelStr);
        String realQlfName = dimLevelStr.lastIndexOf(",0") != -1 ?
                qlfName.substring(0, qlfName.lastIndexOf('.')) : (qlfName != null ? qlfName : fieldName);
        return qualifiedNameToDimLevel.put(realQlfName, dimLevelStr);
    }

    private void makeQlfNameAllLevel(String qlfName) {
        String[] names = qlfName.split("\\.");
        for (String name : names) {
            dimFieldToQualifiedName.put(name, qlfName);
        }
    }

    public String dim_getQlfLevel(String qlfName) {
        return qualifiedNameToDimLevel.get(qlfName);
    }

    public String dim_value() {
        String ret = "";
        for (int i = 0; i < curDims.size(); i++) {
            if (i != 0)
                ret += ",";
            ret += curDims.get(i);
        }
        return ret;
    }

    public String name_toClass(String cblName) {
        String[] parts = cblName.split("-");
        String ret = "";
        for (int i = 0; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }
        return ret;
    }

    public String name_toField(String cblName) {
        String[] parts = cblName.split("-");
        String ret = parts[0].toLowerCase();
        for (int i = 1; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }

        return digitalStart(ret);
    }

    private String digitalStart(String name) {
        if (name != null && name.length() > 0 && name.charAt(0) >= '0' && name.charAt(0) <= '9')
            return "m_" + name;
        return name;
    }

    public String name_getFieldType(String fieldName) {
        String ret = fieldToType.get(fieldName);
        return ret == null ? name_getFieldClsType(fieldName) : ret;
    }

    public String name_getFieldClsType(String fieldName) {
        return fieldToClassType.get(fieldName);
    }

    public String name_setFieldType(String fieldName, String type) {
        return fieldToType.put(fieldName, type);
    }

    public String name_setFieldClsType(String fieldName, String type) {
        return fieldToClassType.put(fieldName, type);
    }

    public String type_getType(String cblType) {
        boolean bString = cblType.indexOf("X(") != -1 || cblType.indexOf("A(") != -1;
        if (bString)
            return "String";
        boolean bFloat = cblType.indexOf("V") != -1;
        if (bFloat)
            return "Double";
        return "Integer";
    }

    public String name_enterClass(String fieldName) {
        return clsLevel.push(fieldName);
    }

    public String name_exitClass() {
        return clsLevel.pop();
    }

    public String name_putInnerField(String fieldName) {
        String qualifiedName = clsLevel.size() == 0 ? null : createQualifedName(fieldName);
        if (qualifiedName != null) {
            fieldToQualifiedName.put(fieldName, qualifiedName);
            makeQlfNameAllLevel(qualifiedName);

        }
        return qualifiedName;
    }

    public String name_delegateName(String fieldName) {
        String ret = fieldToQualifiedName.get(fieldName);
        if (ret == null)
            ret = fieldName;
        return name_delegateName1(ret, null);
    }

    public String name_delegateName1(String fieldName, String dimStr) {
        String ret = fieldName;
        if (dimStr != null) {
            String qlfName = dimFieldToQualifiedName.get(ret);
            ret = qlfName != null ? qlfName : ret;
            String dimLevelStr = dim_getQlfLevel(ret);
            String[] dims = dimStr.split(",");
            String[] names = ret.split("\\.");
            String[] level = dimLevelStr.split(",");
            if (level.length == names.length) {
                String[] lv = new String[names.length - 1];
                System.arraycopy(level, 0, lv, 0, lv.length);
                level = lv;
            }
            ret = "";
            int usedDim = 0;
            for (int i = 0; i < level.length; i++) {
                String node;
                if (level[level.length - 1 - i].equals("0")) {
                    node = names[names.length - 1 - i];
                } else {
                    node = names[names.length - 1 - i] + "[" + dims[dims.length - 1 - usedDim] + "]";
                    usedDim++;
                }
                if (i != 0) {
                    node += ".";
                }
                ret = node + ret;
            }
            if(level.length == 0){
                ret = names[0] + "[" + dims[0] + "]";
            }else {
                for (int i = level.length; i < names.length; i++) {
                    if (ret.length() == 0)
                        ret = names[names.length - i - 1];
                    else
                        ret = names[names.length - i - 1] + "." + ret;
                }
            }
        }
        return ret;
    }

    private String createQualifedName(String fieldName) {
        String ret = "";
        for (String superField : clsLevel) {
            ret += superField + ".";
        }
        return ret + fieldName;
    }

    public String getQualifedName(String fieldName) {
        return fieldToQualifiedName.get(fieldName);
    }

    public String rel_getOper(String cobolOper, String left, String right) {
        String ret = null;
        RelationalOperator oper = RelationalOperator.valueOf(cobolOper);
        switch (oper) {
            case GREATER:
                ret = left + ">" + right;
                break;
            case GREATER_OR_EQUAL:
                ret = left + ">=" + right;
                break;
            case LESS:
                ret = left + "<" + right;
                break;
            case LESS_OR_EQUAL:
                ret = left + "<=" + right;
                break;
            case EQUAL:
                ret = left + "==" + right;
                break;
            case NOT_EQUAL:
                ret = left + "!=" + right;
                break;
            case EQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") == 0";
                break;
            case LESSTHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") < 0";
                break;
            case MORETHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") > 0";
                break;
            case NOTEQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") != 0";
                break;
            case LESSTHANOREQUAL:
                ret = left + "<=" + right;
                break;
            case MORETHANOREQUAL:
                ret = left + ">=" + right;
                break;
        }
        return ret;
    }

    private String getCtxText(CobolParser.ArithmeticExpressionContext ctx){
        String ret = "";
        String tmp = ctx.getText();
        for(int i = 0;i<ctx.getChildCount();i++){
            ret += " " + ctx.getChild(i).getText();
        }
        return ret;
    }
    public String expr_convertExpr(CobolParser.ArithmeticExpressionContext ctx) {
        String cobolExpr = getCtxText(ctx);
        String expression = cobolExpr.replace("**", "^");
        expression = expression.replaceAll("(?<=\\S)-(\\S)", "__$1");
        // Regular expression to match variables of the form identifier(-identifier)*
        Pattern varPattern = Pattern.compile("\\b[a-zA-Z_][a-zA-Z0-9_]*(?:-[a-zA-Z_][a-zA-Z0-9_]*)*\\b");
        Matcher varMatcher = varPattern.matcher(expression);

        // Extract all variables
        List<String> variables = new ArrayList<>();
        while (varMatcher.find()) {
            variables.add(varMatcher.group());
        }
        String ret = expression.replace("__", "-");
        for (String id : variables) {
            id = id.replace("__", "-");
            String fieldName = CobolConstant.isConstant(id) ? "CobolConstant." + id : name_toField(id);
            String[] dims = getDimStringOfVar(cobolExpr, id);
            String dimStr = dims[1];
            if (dimStr == null) {
                ret = ret.replace(id, name_delegateName(fieldName));
            } else {
                int index = cobolExpr.indexOf(":");
                if (index != -1) {
                    int right = cobolExpr.indexOf(')', index);
                    int left = cobolExpr.lastIndexOf('(', index);
                    String range = cobolExpr.substring(left + 1, right);
                    ret = "Util.subvalue(" + fieldName + ",\"" + range + "\")";
                } else {
                    String qlfNameWithDims = name_delegateName1(name_delegateName(fieldName), dimStr);
                    ret = dims[0] + qlfNameWithDims + dims[2];
                }
            }
        }
        return ret.indexOf('^') != -1 ? ExprUtil.convertExpression(ret) : ret;
    }

    private String[] getDimStringOfVar(String cobolExpr, String var) {
        String[] ret = new String[3];
        int index = cobolExpr.indexOf(var);
        String sub = cobolExpr.substring(index + var.length()).trim();
        ret[0] = cobolExpr.substring(0, index);
        ret[2] = sub;
        if (sub.length() != 0 && sub.charAt(0) == '(') {
            int leftIndex = sub.indexOf(')');
            String dimString = sub.substring(1, leftIndex);
            ret[1] = dimString;
            ret[2] = sub.substring(leftIndex + 1);
        }
        return ret;
    }

    public String array_initString(String dims, String val) {
        String[] vals = dims.split(",");
        String str = val;
        for (int i = vals.length - 1; i >= 0; i--) {
            String arr = _initString(vals[i], str);
            str = arr;
        }
        return str;
    }

    public String cbl_getComment(int start, String ctxLine, List<String> lines) {
        String ret = null;
        for (int i = start - 2; i >= 0; i--) {
            String line = lines.get(i);
            String trim = line.trim();
            if (trim.startsWith("*>")) {
                if (ret == null)
                    ret = "//" + trim.substring(2);
                else
                    ret += "\n//" + trim.substring(2);
            } else if (trim.length() != 0)
                break;
        }
        return ret;
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
