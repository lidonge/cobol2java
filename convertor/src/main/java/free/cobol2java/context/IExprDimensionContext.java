package free.cobol2java.context;

import java.util.HashMap;
import java.util.Map;

import static free.cobol2java.context.IExprBaseContext.toClassName;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprDimensionContext extends IExprPhysicalContext, IExprEnvContext{
    default String addDimToQlfName(String theJavaQlfName, String dimStr) {
        String ret= null;
        Map<String, Number> javaFieldNameToDim = null;
        String[] dims = dimStr.split(",");
        //use max name access array, there is an ambiguity here, but can not avoid
        String javaQlfName = getJavaQlfNameWithLeaf(theJavaQlfName);
        if(javaQlfName == null){
            //in copy book
            javaFieldNameToDim = new HashMap<>();
            addFullDims(javaFieldNameToDim, theJavaQlfName);
            javaQlfName = theJavaQlfName;
        }else {
            String[] leafs = javaQlfName.split("\\|");
            javaQlfName = findMaxMatchingPrefix(leafs, theJavaQlfName);
            javaFieldNameToDim = getJavaFieldNameToDim();
        }
        ret = _addDimToQlfName(javaFieldNameToDim, javaQlfName, dims);
        return ret;
    }

    private void addFullDims(Map<String, Number> javaFieldNameToDim, String theJavaQlfName){
        String[] names = theJavaQlfName.split("\\.");
        IExprDimensionContext context = this;
        String prevName = null;
        String curQlfName = null;
        String curQlfNameInCtx = null;
        for(String name : names){
            Number dim = context.getJavaFieldNameToDim().get(name);
            if(curQlfName == null)
                curQlfName = prevName;
            else
                curQlfName += "." + prevName;
            if(curQlfNameInCtx == null)
                curQlfNameInCtx = prevName;
            else
                curQlfNameInCtx += "." + prevName;

            if (dim != null) {
                javaFieldNameToDim.put(name,dim);
            }else{
                if(curQlfNameInCtx == null){
                    javaFieldNameToDim.put(name, 0);
                }else {
                    if( Character.isUpperCase(curQlfNameInCtx.charAt(0))) {
                        context = context.getCopybookContexts().get(curQlfNameInCtx);
                    }else {
                        String copybookClsName = context.getJavaQlfFieldToSimpleType().get(curQlfNameInCtx);

                        String copyFieldName = context.getQlfNameToCopyFieldName().get(curQlfNameInCtx);
                        if (copyFieldName != null) {
                            copybookClsName = toClassName(copyFieldName);
                            curQlfNameInCtx = copyFieldName;
                        }
                        context = context.getCopybookContexts().get(copybookClsName);
                    }
                    dim = context.getJavaFieldNameToDim().get(name);
                    javaFieldNameToDim.put(name, dim);
                }
            }
            prevName = name;
        }
    }

    // Find the longest matching prefix of multiple strings separated by '.'
    private static String findMaxMatchingPrefix(String[] strings, String ref) {
        // If the list is empty or null, return an empty string
        if (strings == null || strings.length == 0) {
            return "";
        }
        String ret = ref;

        int maxLen = 0;
        for (int i = 0; i < strings.length; i++) {
            String s = strings[i];
            if(s.startsWith(ref)) {
                if(s.length() > maxLen) {
                    maxLen = s.length();
                    ret = s;
                }
            }
        }

        return ret;
    }
    private static String _addDimToQlfName(Map<String, Number> javaFieldNameToDim, String javaQlfName, String[] dims) {
        String ret = null;
        String[] names = javaQlfName.split("\\.");
        int dimIndex = 0;
        for (String name:names) {
            Number dim = javaFieldNameToDim.get(name);
            if (dim != null && dim.intValue() != 0) {
                //access part dimensions of the multiple dimension array
                if (dimIndex == dims.length) {
                    break;
                }
                name = name + "[" + dims[dimIndex++] + "]";
            }
            if (ret == null)
                ret = name;
            else
                ret += "." + name;
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
