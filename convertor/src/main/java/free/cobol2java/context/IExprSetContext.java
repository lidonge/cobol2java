package free.cobol2java.context;

import java.util.Map;

import static free.cobol2java.context.IExprBaseContext.isBaseType;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprSetContext extends IExprPhysicalContext, IExprEnvContext {

    default String setQlfNameToCopyFieldName(String qlfName, String copyName) {
        createMultiQlfName(copyName,getCopyFieldNameToQlfName(),qlfName);
        return getQlfNameToCopyFieldName().put(qlfName, copyName);
    }

    default String setInnerClsNameToCopyName(String innerClsName, String copyName) {
        return getInnerClsNameToCopybookName().put(innerClsName, copyName);
    }

    /**
     * Set when field defined(PIC or 01 FIELD-NAME.)
     *
     * @param fieldName
     * @param type
     * @return
     */
    default String name_setFieldType(String fieldName, String type) {
        if(!isBaseType(type)) {
            String fullClsName = createFullClassName(type);
            if (fullClsName != null) {
                getJavaQlfFieldToFullType().put(fieldName, fullClsName + "." + type);
            }
        }
        return getJavaQlfFieldToSimpleType().put(fieldName, type);
    }

    private String createFullClassName(String type) {
        String ret = null;
        if (getCopybookContexts().get(type) != null ||
                getCopybookContexts().get(getInnerClsNameToCopybookName().get(type)) != null) {
        } else if (getClsLevel().size() != 0) {
            for (String path : getClsLevel()) {
                path = IExprBaseContext.capitalizeFirstLetter(path);
                if (type.equals(getInnerClsNameToCopybookName().get(path))) {
                    continue;
                }
                if (ret == null) {
                    ret = path;
                } else {
                    ret += "." + path;
                }
            }
        }
        return ret;
    }

    default Number dim_putFieldDim(String fieldName, Number dim) {
        return getJavaFieldNameToDim().put(fieldName, dim);
    }

    default String name_putInnerField(String fieldName) {
        return name_putInnerField1(fieldName, null);
    }

    default String name_putInnerField1(String fieldName, String isSubCopybook) {
        String qualifiedName = fieldName;
        int levels = getClsLevel().size();
        if (levels != 0) {
            if (isSubCopybook != null && !isSubCopybook.equals("null")) {
                qualifiedName = createQualifiedName("");
                if(levels > 1)
                    fieldName = qualifiedName.substring(qualifiedName.lastIndexOf(".")+1);
                else
                    fieldName = qualifiedName;
            } else {
                qualifiedName = createQualifiedName(fieldName);
            }

        }
        Map<String, String> javaFieldToQualifiedName = getJavaFieldToQualifiedName();
        createMultiQlfName(fieldName, javaFieldToQualifiedName, qualifiedName);
        makeQlfNameAllLevel(qualifiedName);
        return qualifiedName;
    }

    private void createMultiQlfName(String fieldName, Map<String, String> javaFieldToQualifiedName, String qualifiedName) {
        String oldQlfName = javaFieldToQualifiedName.get(fieldName);
        if (oldQlfName == null || oldQlfName.equals(qualifiedName)) {
            oldQlfName = qualifiedName;
        } else if(oldQlfName.indexOf(qualifiedName) == -1){
            oldQlfName += "|" + qualifiedName;
        }
        javaFieldToQualifiedName.put(fieldName, oldQlfName);
    }

    private void makeQlfNameAllLevel(String qlfName) {
        String[] names = qlfName.split("\\.");
        for (String name : names) {
            String lastName = getJavaFieldToQlfNameWithLeaf().get(name);
            if (lastName == null ) {
                getJavaFieldToQlfNameWithLeaf().put(name, qlfName);
            }else{
                String[] leafs = lastName.split("\\|");
                String newLastName = null;
                boolean isDeal = false;
                for(String leaf:leafs){
                    if(!isDeal) {
                        int lastLevel = countCharacter(leaf, '.') + 1;
                        if (lastLevel < names.length) {
                            if (qlfName.indexOf(leaf) != -1) {
                                //change the leaf to the qlfName
                                leaf = qlfName;
                                isDeal = true;
                            }
                        } else if (lastLevel == names.length) {

                            if(newLastName == null)
                                newLastName = qlfName;
                            else
                                newLastName += "|" + qlfName;
                            isDeal = true;
                        }
                    }
                    if(newLastName == null)
                        newLastName = leaf;
                    else
                        newLastName += "|" + leaf;
                }
                getJavaFieldToQlfNameWithLeaf().put(name, newLastName);
            }
        }
    }
    private static int countCharacter(String input, char character) {
        int count = 0;
        for (int i = 0; i < input.length(); i++) {
            if (input.charAt(i) == character) {
                count++;
            }
        }
        return count;
    }
    private String createQualifiedName(String fieldName) {
        String ret = null;
        boolean needLast = fieldName.length() != 0;
        for (String superField : getClsLevel()) {
            if (ret == null)
                ret = superField;
            else {
                ret += "." + superField;
            }
        }
        if (needLast)
            ret += "." + fieldName;
        return ret;
    }

}
