package free.cobol2java.context;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprValueContext extends IExprEnvContext, IExprPhysicalContext{


    default String type_getType(String cblType) {
        boolean bString = cblType.indexOf("X(") != -1 || cblType.indexOf("A(") != -1;
        if (bString)
            return "String";
        boolean bFloat = cblType.indexOf("V") != -1;
        if (bFloat)
            return "Double";
        return "Integer";
    }
    default String value_fixFullArraySet(String setName, String setValue){
        String ret = null;
        if(!setName.endsWith("]")) {
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
    default String value_fix(String left, Object right) {
        String ret = right + "";
        String leftType = getTypeByQlfName(null, left);
        String rightType = getTypeByQlfName(null, ret);
        if (leftType == null) {
            debugPoint();
            leftType = "";
        }
        if (rightType == null) {
            debugPoint();
            rightType = "";
        }
        if (isBaseType(leftType) && isBaseType(rightType)) {
            if (!leftType.equals("String") && ret.indexOf("\"") != -1) {
                ret = ret.replace("\"", "");
            } else if (leftType.equals("Double") && ret.indexOf(".") == -1) {
                ret = right + "d";
            }
        } else {
            if(left.startsWith("Util.subvalue(")){
                left = left.substring("Util.subvalue(".length(), left.length()-1);
                int idx = left.indexOf(",");
                String var = left.substring(0,idx);
                String rang = left.substring(idx+1);
                ret = "Util.copyObject(" + right + "," + var+","+rang + ")";
            }else
                ret = "Util.copyObject(" + right + "," + left + ")";
        }
        return ret;
    }

    private String getTypeByQlfName(String parent, String left) {
        String ret = getJavaQlfFieldToType().get(left);
        int index = left.indexOf(".");
        if (ret == null && index != -1) {
            String clsFieldName = left.substring(0, index);
            String clsType = getJavaQlfFieldToType().get(parent == null ? clsFieldName : parent + "." + clsFieldName);

            String innerClsName = getInnerClsNameToCopybookName().get(clsType);
            if (innerClsName == null) {
                //type and field same name
                innerClsName = clsType;
            }
            ExprContext exprContext = getCopybookContexts().get(innerClsName);
            if (exprContext != null) {
                String fieldName = innerClsName.substring(0, 1).toLowerCase() + innerClsName.substring(1);
                String qlfNameInContext = left.replace(clsFieldName, fieldName);
                ret = exprContext.getJavaQlfFieldToType().get(qlfNameInContext);
            } else {
                ret = getTypeByQlfName(clsFieldName, left.substring(index + 1));
            }
        }
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
