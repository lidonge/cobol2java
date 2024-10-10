package free.cobol2java.context;

import free.servpp.mustache.ILogable;

import java.util.Map;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprNameContext extends ILogable, IExprEnvContext,IExprPhysicalContext,
        IExprBaseContext, ICopybookContext, IExprCtxHandler,
        IExprDimensionContext{
    default String name_qlfName(String fieldName, String ofCopies) {
        if(fieldName.equals("fmCurrCod")){
            debugPoint();
        }
        String ret = null;
        boolean isInCopy = ofCopies != null && ofCopies.length() != 0 && !"null".equals(ofCopies);
        if (isInCopy) {
            ret = getQlfNameWithOfCopies(fieldName, ofCopies);
        }else{
            ret = getQlfNameWithoutOfCopy(fieldName);
        }
        return ret;
    }

    private String getQlfNameWithOfCopies(String fieldName, String ofCopies) {
        String ret = null;
        String[] ofIds = ofCopies.split("\\.");

        //A OF B OF C
        for (int i = ofIds.length-1; i > 0; i--) {
            String theFieldName = ofIds[i-1];
            String theOfCopy = ofIds[i];
            String qlfName = getQlfNameWithOneOfCopy(theFieldName,theOfCopy);
            if(ret == null){
                ret = qlfName;
            }else{
                ret = ret + "." + qlfName;
            }
        }
        //A OF B
        String last = getQlfNameWithOneOfCopy(fieldName, ofIds[0]);
        if(ret == null){
            ret = last;
        }else{
            ret +="." + last;
        }
        return ret;
    }

    private String getQlfNameWithOneOfCopy(String fieldName, String ofCopy) {
        String ret;
        String ofCopyField = name_toField(ofCopy);
        String qlfNameInCopybook = _getQlfName(fieldName, ofCopyField);

        if(qlfNameInCopybook == null){
            debugPoint();
        }
        int index = qlfNameInCopybook.indexOf(ofCopyField);
        int beginIndex = index + ofCopyField.length();
        //01 A. COPY B.
        //MOVE C OF A TO X.
        if(index == -1){
            beginIndex = qlfNameInCopybook.indexOf(".");
        }
        if(beginIndex == -1) {
            ret = getQlfNameWithoutOfCopy(ofCopyField) + "." + qlfNameInCopybook;
        }else {
            //FIXME 03  CICIFCIF-DEF1-AREA     REDEFINES    CICIFCIF-AREA.
            ret = getQlfNameWithoutOfCopy(ofCopyField) +
                    qlfNameInCopybook.substring(beginIndex);
        }
        return ret;
    }

    private String getQlfNameWithoutOfCopy(String fieldName) {
        String ret;
        String qlfName = getJavaFieldToQualifiedName().get(fieldName);
        if(qlfName != null) {
            //Local field
            ret = qlfName;
        }
        else{
            if(getJavaQlfFieldToType().get(fieldName) != null) {
                //fieldName is defined in main cbl and fieldName is copybook field
                String javaFieldName = getCopyFieldNameToJavaFileName().get(fieldName);
                //javaFieldName == null, same name copy book;else copy75
                ret = javaFieldName != null ? javaFieldName: fieldName;
            }
            else {
                //fieldName is inside copybook
                ret = replaceQlfNameInsideCopybook(fieldName, false);
            }
        }
        return ret;
    }

    private String replaceQlfNameInsideCopybook(String fieldName, boolean isMiddleName) {
        String ret = null;
        IExprNameContext exprContext = getExprContext(fieldName,isMiddleName);
        if(exprContext == null){
            getLogger().error("Error Undefined middle({}) field:{}", isMiddleName, fieldName);
            return "UNDEFINED_FIELD_"+fieldName;
        }
        String theFieldName = exprContext.getCopyFieldNameToJavaFileName().get(fieldName);
        if(!isMiddleName || theFieldName == null){
            theFieldName = fieldName;
        }
        String qlfNameInsideCopybook = exprContext.getJavaFieldToQualifiedName().get(theFieldName);
        if(qlfNameInsideCopybook == null){
            //copy75
            qlfNameInsideCopybook = IExprBaseContext.lowerFirstLetter(
                    exprContext.getCopyBookName())+"."+theFieldName;
        }
        int index = qlfNameInsideCopybook.indexOf(".");

        String copyFieldName = qlfNameInsideCopybook.substring(0,index);
        String javaFieldName = getCopyFieldNameToJavaFileName().get(copyFieldName);
        if(javaFieldName != null) {
            //copy75
        }else{
            //copyFieldName may be same with java fieldName, or inside other copybook
            javaFieldName = getJavaFieldToQualifiedName().get(copyFieldName);
            if(javaFieldName == null){
                //inside other copybook
            }
        }

        if(javaFieldName == null) {
            if(Character.isUpperCase(copyFieldName.charAt(0))){
                //constant
                ret = qlfNameInsideCopybook;
            }else{
                //inside other copybook
                ret = replaceQlfNameInsideCopybook(copyFieldName, true) + qlfNameInsideCopybook.substring(index);
//                ret = qlfNameInsideCopybook;
                if(ret == null)
                    getLogger().error("Error The field {} define not found." , fieldName);
            }
        }else {
            String qlfName = getJavaFieldToQualifiedName().get(javaFieldName);
            if(qlfName == null){
                //copy75
                qlfName = javaFieldName;
            }
            ret = qlfName + qlfNameInsideCopybook.substring(index);
        }

        return ret;
    }

    private String _getQlfName(String fieldName, String ofCopyField) {
        String ret = null;
        IExprNameContext exprContext = getOfCopyContext(ofCopyField);
        if(exprContext == null)
            ret = getJavaFieldToQualifiedName().get(fieldName);
        else {
            ret = exprContext.name_qlfName(fieldName,null);
        }
        return ret;
    }

    default String name_qlfUdfNameWithDim(String javaQlfName, String dimStr) {
        String ret = null;
        String delegate = javaQlfName.substring(javaQlfName.lastIndexOf('.') + 1);
        IExprNameContext exprContext = getExprContext(delegate, false);
        if(exprContext == null){
            debugPoint();
        }
        ret = exprContext.name_qlfNameWithDim(javaQlfName, dimStr);
//        ret = nestedQualifiedName(ret);

        return ret;
    }

    default String name_qlfNameWithDim(String theJavaQlfName, String dimStr) {
        String ret = null;

        if (dimStr == null) {
            ret = theJavaQlfName;
        } else {
            ret = addDimToQlfName(theJavaQlfName, dimStr);
        }
//        ret = nestedQualifiedName(ret);

        return ret;
    }


    default String name_getFieldType(String fieldName) {
        String ret = getJavaQlfFieldToType().get(fieldName);
        if (ret != null && getInnerClsNameToCopybookName().get(ret) != null) {
            ret = getInnerClsNameToCopybookName().get(ret);
        }
        if(ret == null) {
            ret = name_getFieldClsType(fieldName);
            if(ret == null){
                int index = fieldName.lastIndexOf('.');
                if(index != -1){
                    fieldName = fieldName.substring(index+1);
                }
                IExprNameContext exprContext = getExprContext(fieldName, false);
                if(exprContext != null)
                    ret = exprContext.name_getFieldType(exprContext.name_qlfName(fieldName,null));
            }
        }
        return ret;
    }

    default String name_getFieldClsType(String fieldName) {
        return getFieldToClassType().get(fieldName);
    }
}
