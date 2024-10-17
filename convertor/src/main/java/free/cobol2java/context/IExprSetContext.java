package free.cobol2java.context;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprSetContext extends IExprPhysicalContext,IExprEnvContext{

    default String setFieldNameToCopyFieldName(String fieldName,String copyName) {
        getCopyFieldNameToJavaFileName().put(copyName,fieldName);
        return getJavaFieldNameToCopyFieldName().put(fieldName, copyName);
    }

    default String setInnerClsNameToCopyName(String innerClsName, String copyName) {
        return getInnerClsNameToCopybookName().put(innerClsName, copyName);
    }
    /**
     * Set when field defined(PIC or 01 FIELD-NAME.)
     * @param fieldName
     * @param type
     * @return
     */
    default String name_setFieldType(String fieldName, String type) {
        String fullClsName = createFullClassName(type);
        if(fullClsName != null){
            getFieldToClassType().put(fieldName, fullClsName+"."+type);
        }
        return getJavaQlfFieldToType().put(fieldName, type);
    }
    private String createFullClassName(String type){
        String ret = null;
        if(getCopybookContexts().get(type) != null ||
                getCopybookContexts().get(getInnerClsNameToCopybookName().get(type))!= null)
        {
        }
        else if(getClsLevel().size() != 0){
            for(String path:getClsLevel()) {
                path = IExprBaseContext.capitalizeFirstLetter(path);
                if(type.equals(getInnerClsNameToCopybookName().get(path))){
                    continue;
                }
                if(ret ==null){
                    ret = path;
                }else{
                    ret +="."+path;
                }
            }
        }
        return ret;
    }
    default Number dim_putFieldDim(String fieldName, Number dim) {
        return getJavaFieldNameToDim().put(fieldName, dim);
    }
    default String name_setFieldClsType(String fieldName, String type) {
        return getFieldToClassType().put(fieldName, type);
    }
    default String name_putInnerField(String fieldName) {
        return name_putInnerField1(fieldName, null);
    }
    default String name_putInnerField1(String fieldName, String isSubCopybook) {
        String qualifiedName = null;

        if (isSubCopybook != null && !isSubCopybook.equals("null")) {
            //copy75
            qualifiedName = null;
        } else {
            qualifiedName = getClsLevel().size() == 0 ? fieldName : createQualifiedName(fieldName);
            String oldQlfName = getJavaFieldToQualifiedName().get(fieldName);
            if(oldQlfName == null || oldQlfName.equals(qualifiedName)){
                oldQlfName =qualifiedName;
            }else {
                oldQlfName += "|" + qualifiedName;
            }
            getJavaFieldToQualifiedName().put(fieldName, oldQlfName);
            makeQlfNameAllLevel(qualifiedName);
        }
        return qualifiedName;
    }

    private void makeQlfNameAllLevel(String qlfName) {
        String[] names = qlfName.split("\\.");
        for (String name : names) {
            String lastName = getJavaFieldToQlfNameWithLeaf().get(name);
            if (lastName == null || lastName.length() < qlfName.length())
                getJavaFieldToQlfNameWithLeaf().put(name, qlfName);
        }
    }

    private String createQualifiedName(String fieldName) {
        String ret = "";
        for (String superField : getClsLevel()) {
            ret += superField + ".";
        }
        return ret + fieldName;
    }

}
