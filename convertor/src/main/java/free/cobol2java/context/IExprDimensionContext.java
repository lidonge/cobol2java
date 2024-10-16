package free.cobol2java.context;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprDimensionContext extends IExprPhysicalContext, IExprEnvContext{
    default String addDimToQlfName(String theJavaQlfName, String dimStr) {
        String ret= null;
        String[] dims = dimStr.split(",");
        //use max name access array, there is an ambiguity here, but can not avoid
        String javaQlfName = getJavaQlfNameWithLeaf(theJavaQlfName);
        if(javaQlfName == null){
            //in copy book
            String javaFieldName = theJavaQlfName.substring(0,theJavaQlfName.indexOf("."));
            String copybookClsName = getJavaQlfFieldToType().get(javaFieldName);
            String copyFieldName = getJavaFieldNameToCopyFieldName().get(javaFieldName);
            if(copyFieldName != null){
                copybookClsName = getJavaQlfFieldToType().get(copyFieldName);
            }
            IExprNameContext exprNameContext = getCopybookContexts().get(copybookClsName);
            return exprNameContext.addDimToQlfName(theJavaQlfName,dimStr);
        }
        String[] theNames = theJavaQlfName.split("\\.");
        String[] names = javaQlfName.split("\\.");
        int dimIndex = 0;
        for (int i = 0;i<names.length;i++) {
            String name = names[i];
            String theName = i < theNames.length ? theNames[i] : name;
            Number dim = getJavaFieldNameToDim().get(name);
            if (dim != null && dim.intValue() != 0) {
                //access part dimensions of the multiple dimension array
                if (dimIndex == dims.length) {
                    break;
                }
                theName = theName + "[" + dims[dimIndex++] + "]";
            }
            if (ret == null)
                ret = theName;
            else
                ret += "." + theName;
        }
        return ret;
    }
    default String getJavaQlfNameWithLeaf(String javaQlfName) {
        return getJavaFieldToQlfNameWithLeaf().get(javaQlfName.substring(javaQlfName.lastIndexOf(".") + 1));
    }

    default String[] getDimStringOfVar(String cobolExpr, String var) {
        String[] ret = new String[3];
        int index = cobolExpr.indexOf(var);

        String sub = cobolExpr.substring(index + var.length()).trim();
        if(index == -1){
            debugPoint();
        }
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
}
