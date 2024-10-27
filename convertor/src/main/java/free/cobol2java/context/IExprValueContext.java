package free.cobol2java.context;

import free.cobol2java.java.CobolConstant;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprValueContext extends IExprEnvContext, IExprNameContext {


    default String type_getType(String cblType) {
        boolean bString = cblType.indexOf("X(") != -1 || cblType.indexOf("A(") != -1;
        if (bString)
            return "String";
        boolean bFloat = cblType.indexOf("V") != -1;
        if (bFloat)
            return "Double";
        return "Integer";
    }

    default String value_fixFullArraySet(String setName, String setValue) {
        String ret = null;
        if (!setName.endsWith("]")) {
            int index = setName.lastIndexOf(".");
            String fieldName = setName;
            if (index != -1)
                fieldName = setName.substring(index + 1);

            Number number = getJavaFieldNameToDim().get(fieldName);
            if (number != null && number.intValue() > 0) {
                ret = "fix";
            }
        }
        return ret;
    }

    private String getRightConstType(String oper) {
        String ret = null;
        if (oper.indexOf("\"") != -1) {
            ret = "String";
        } else if (isInteger(oper)) {
            ret = "Integer";
        } else {
            if (isDouble(oper)) {
                ret = "Double";
            }
        }
        return ret;
    }

    static boolean isDouble(String s) {
        try {
            Double.parseDouble(s);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    static boolean isInteger(String s) {
        try {
            Integer.parseInt(s);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    default String value_fix(String left, Object right) {
        String ret = null;
        String sRight = right+"";

        getEnvironment().setVar("leftIsBase","null");
        if (left.startsWith("Util.subvalue(")) {
            ret = fixSubvalue(left, right);
        } else {
            String leftType = name_getFullFieldType(removeDim(left));
            sRight = removeDim(sRight);
            boolean isRightLengthOf = sRight.startsWith("Util.lengthOf(");
            boolean isConstant = sRight.startsWith("CobolConstant.");
            boolean isSubvalue = sRight.startsWith("Util.subvalue(");
            boolean isUndefined = sRight.startsWith("UNDEFINED_");
            String rightType = isRightLengthOf ? "Integer":
                    isConstant ? "CobolConstant" :
                    isSubvalue ? "String":
                    isUndefined ? "Object":getRightConstType(sRight);
            if (rightType == null)
                rightType = name_getFullFieldType(sRight);

            if (leftType.equals(rightType)) {
                getEnvironment().setVar("leftIsBase","leftIsBase");
                if (leftType.equals("Double") && sRight.indexOf(".") == -1) {
                    ret = right + "d";
                }else
                    ret = right+"";
            } else {
                ret = fixBaseType(right+"", leftType, rightType);

                if (ret == null) {
                    String IsCorrMove = getEnvironment().getVar("IsCorrMove") + "";
                    if ("IsCorrMove".equals(IsCorrMove))
                        ret = "Util.copySameField(" + right + "," + left + ")";
                    else {
                        if(isBaseType(leftType)){
                            ret = "Util.copyCastTo"+leftType+"(" + right+ ")";
                        }else
                            ret = "Util.copyCast(" + right + "," + left + ")";
                    }
                }
            }
        }
        return ret;
    }

    private String removeDim(String left){
        String ret = "";
        String[] parts = left.split("\\[");
        for(String part:parts){
            int index = part.indexOf("]");
            if(index != -1)
                part = index == part.length() - 1 ? "":part.substring(index + 1);
            ret += part;
        }
        return ret;
    }

    private String fixBaseType(String sRight, String leftType, String rightType) {
        String ret = null;
        if (isBaseType(leftType) ) {
            getEnvironment().setVar("leftIsBase","leftIsBase");
            if(isBaseType(rightType) || "CobolConstant".equals(rightType)){
                sRight = sRight.replace("\"", "");
                if (leftType.equals("Integer")) {
                    if( isInteger(sRight))
                        ret = sRight;
                    else if(sRight.equals("CobolConstant.ZEROS") || sRight.equals("CobolConstant.ZERO") ||
                            sRight.equals("CobolConstant.ZEROES") || sRight.equals("CobolConstant.ZEROE")){
                        ret = "0";
                    }
                } else if (leftType.equals("Double") ) {
                    if(isDouble(sRight))
                        ret = sRight + "d";
                    else if(sRight.equals("CobolConstant.ZEROS") || sRight.equals("CobolConstant.ZERO") ||
                            sRight.equals("CobolConstant.ZEROES") || sRight.equals("CobolConstant.ZEROE")){
                        ret = "0d";
                    }
                } else if (leftType.equals("String")) {
                    if(sRight.equals("CobolConstant.ZEROS") || sRight.equals("CobolConstant.ZERO") ||
                            sRight.equals("CobolConstant.ZEROES") || sRight.equals("CobolConstant.ZEROE")){
                        ret = "Util.copyInitString(\"0\")";
                    }else if(sRight.equals("CobolConstant.SPACE") || sRight.equals("CobolConstant.SPACES")){
                        ret = "Util.copyInitString(\" \")";
                    }else
                        ret = sRight + "+\"\"";
                }
            }
        }
        return ret;
    }

    private String fixSubvalue(String left, Object right) {
        getEnvironment().setVar("leftIsBase","leftIsBase");
        String ret;
        left = left.substring("Util.subvalue(".length(), left.length() - 1);
        int idx = left.indexOf(",");
        String var = left.substring(0, idx);
        String rang = left.substring(idx + 1);
        ret = "Util.copyObject(" + right + "," + var + "," + rang + ")";
        getEnvironment().setVar("setName",var);
        return ret;
    }

    private boolean isBaseType(String type) {
        boolean ret = false;
        switch (type) {
            case "String":
            case "Integer":
            case "Double":
                ret = true;
                break;
        }
        return ret;
    }
}
