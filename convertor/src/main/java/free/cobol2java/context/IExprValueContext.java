package free.cobol2java.context;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprValueContext extends IExprEnvContext, IExprNameContext {

    default String type_replace(String name, String orgType) {
        return TypeReplacement.replace(name,orgType);
    }
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

    default String value_fixBase(String propType, Object value){
        String sRight = value +"";
        String rightType = getRightType(sRight);
        String ret = fixBaseType(sRight,propType,rightType, false);
        return ret;
    }
    default String value_fix(String left, Object right) {
        String ret = null;
        String sRight = right+"";
        if(sRight.startsWith("Function."))
                return sRight;
        getEnvironment().setVar("leftIsBase","null");
        if (left.startsWith("Util.subvalue(")) {
            ret = fixSubvalue(left, right);
        } else {
            String leftType = name_getFullFieldType(removeDim(left));
            sRight = removeDim(sRight);
            String rightType = getRightType(sRight);
            boolean rightIsAField = false;
            if (rightType == null) {
                rightType = name_getFullFieldType(sRight);
                rightIsAField = true;
            }

            if(leftType == null)
                getLogger().error("Error can not detect left type of {}." ,left);
            if(rightType == null)
                getLogger().error("Error can not detect right type of {}.",sRight);

            if (leftType.equals(rightType)) {
                getEnvironment().setVar("leftIsBase","leftIsBase");
                if (leftType.equals("Double") && sRight.indexOf(".") == -1) {
                    ret = right + "d";
                }else
                    ret = right+"";
            } else {
                ret = fixBaseType(right+"", leftType, rightType,rightIsAField);

                if (ret == null) {
                    String IsCorrMove = getEnvironment().getVar("IsCorrMove") + "";
                    if ("IsCorrMove".equals(IsCorrMove))
                        ret = "Util.copySameField(" + right + "," + left + ")";
                    else {
                        if(isBaseType(leftType)){
                            ret = "Util.copyCastTo"+leftType+"(" + right+ ")";
                        }else
                            ret = "Util.copyCaster().copyCast(" + right + "," + left + ")";
                    }
                }
            }
        }
        return ret;
    }

    private String getRightType(String sRight) {
        boolean isRightLengthOf = sRight.startsWith("Util.lengthOf(");
        boolean isConstant = sRight.startsWith("CobolConstant.");
        boolean isSubvalue = sRight.startsWith("Util.subvalue(");
        boolean isUndefined = sRight.startsWith("UNDEFINED_");
        String rightType = isRightLengthOf ? "Integer":
                isConstant ? "CobolConstant" :
                isSubvalue ? "String":
                isUndefined ? "Object":getRightConstType(sRight);
        return rightType;
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

    private String fixBaseType(String sRight, String leftType, String rightType, boolean rightIsAField) {
        String ret = null;

        if (isBaseType(leftType) ) {
            getEnvironment().setVar("leftIsBase","leftIsBase");
            if(isBaseType(rightType) || "CobolConstant".equals(rightType)){
                sRight = sRight.replace("\"", "");
                if (leftType.equals("Integer")) {
                    if( isInteger(sRight))
                        ret = Integer.parseInt(sRight)+"";
                    else if(isZero(sRight)||isSpace(sRight)){
                        ret = "0";
                    }else if(rightIsAField){
                        ret = "Util.copyCastToInteger("+sRight+")";
                    }else
                        ret = "Util.copyCastToInteger(\""+sRight+"\")";
                } else if (leftType.equals("Double") ) {
                    if(isDouble(sRight))
                        ret = sRight + "d";
                    else if(isZero(sRight)||isSpace(sRight)){
                        ret = "0d";
                    }else if(rightIsAField){
                        ret = "Util.copyCastToDouble("+sRight+")";
                    }else
                        ret = "Util.copyCastToDouble(\""+sRight+"\")";
                } else if (leftType.equals("String")) {
                    boolean isAll = false;
                    if (sRight.equals("CobolConstant.ALL")){
                        sRight = getEnvironment().getVar("constantAllValue")+"";
                        isAll = true;
                    }
                    if(isZero(sRight)){
                        ret = "Util.copyInitString("+isAll+",\"0\")";
                    }else if(isSpace(sRight)){
                        ret = "Util.copyInitString("+isAll+", \" \")";
                    }else if(rightType.equals("String")){
                        if(rightIsAField)
                            ret = sRight;
                        else
                            ret = "\"" + sRight + "\"";
                        if(isAll)
                            ret = "Util.copyInitString("+isAll+"," +ret+")";
                    }else {
                        ret = sRight + "+\"\"";
                    }
                }
            }
        }
        return ret;
    }

    private static boolean isSpace(String sRight) {
        return sRight.equals("CobolConstant.SPACE") || sRight.equals("CobolConstant.SPACES");
    }

    private static boolean isZero(String sRight) {
        return sRight.equals("CobolConstant.ZEROS") || sRight.equals("CobolConstant.ZERO") ||
                sRight.equals("CobolConstant.ZEROES") || sRight.equals("CobolConstant.ZEROE");
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

    default boolean isBaseType(String type) {
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
