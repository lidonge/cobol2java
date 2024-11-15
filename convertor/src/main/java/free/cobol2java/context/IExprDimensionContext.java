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
            javaQlfName = findMaxMatchingPrefix(leafs);
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
        for(String name : names){
            Number dim = context.getJavaFieldNameToDim().get(name);
            if(curQlfName == null)
                curQlfName = prevName;
            else
                curQlfName += "." + prevName;
            if (dim != null) {
                javaFieldNameToDim.put(name,dim);
            }else{
                String copybookClsName = context.getJavaQlfFieldToSimpleType().get(curQlfName);

                String copyFieldName = context.getQlfNameToCopyFieldName().get(curQlfName);
                if(copyFieldName != null){
                    copybookClsName = toClassName(copyFieldName);
                }
                context = context.getCopybookContexts().get(copybookClsName);
                dim = context.getJavaFieldNameToDim().get(name);
                javaFieldNameToDim.put(name,dim);
            }
            prevName = name;
        }
    }

    // Find the longest matching prefix of multiple strings separated by '.'
    private static String findMaxMatchingPrefix(String[] strings) {
        // If the list is empty or null, return an empty string
        if (strings == null || strings.length == 0) {
            return "";
        }

        // Split the first string to use it as a reference
        String[] referenceParts = strings[0].split("\\.");

        // StringBuilder to store the matching prefix
        StringBuilder matchingPrefix = new StringBuilder();

        // Iterate through each part of the reference string
        for (int i = 0; i < referenceParts.length; i++) {
            String currentPart = referenceParts[i];  // Get the current part
            boolean allMatch = true;

            // Compare the current part with the corresponding part in all other strings
            for (String str : strings) {
                String[] parts = str.split("\\.");  // Split each string by '.'

                // If the current part doesn't exist or doesn't match, stop the comparison
                if (i >= parts.length || !parts[i].equals(currentPart)) {
                    allMatch = false;
                    break;
                }
            }

            // If all strings match for this part, append it to the result
            if (allMatch) {
                if (matchingPrefix.length() > 0) {
                    matchingPrefix.append(".");  // Add a dot separator if it's not the first part
                }
                matchingPrefix.append(currentPart);
            } else {
                break;  // Stop when a mismatch is found
            }
        }

        // Return the final matching prefix
        return matchingPrefix.toString();
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
