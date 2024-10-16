package free.cobol2java.context;


import free.cobol2java.util.CobolConstant;
import free.servpp.logger.ILogable;

import java.util.Arrays;
import java.util.Map;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprNameContext extends ILogable, IExprEnvContext,IExprPhysicalContext,
        IExprBaseContext, ICopybookContext, IExprCtxHandler,
        IExprDimensionContext{
    default boolean name_isConst(String name){
        return CobolConstant.isConstant(name);
    }
    default String name_qlfName(String fieldName, String ofCopies) {
        return name_qlfName(fieldName,ofCopies,null);
    }
    default String name_qlfName(String fieldName, String ofCopies, String localOf) {
        if(fieldName.equals("SBCA-CNT")){
            debugPoint();
        }
        String ret = null;
        boolean isInCopy = ofCopies != null && ofCopies.length() != 0 && !"null".equals(ofCopies);
        if (isInCopy) {
            ret = getQlfNameWithOfCopies(fieldName, ofCopies);
        }else{
            ret = getQlfNameWithoutOfCopy(fieldName,localOf);
        }
        return ret;
    }

    private String getQlfNameWithOfCopies(String fieldName, String ofCopies) {
        String ret = null;
        String[] ofIds = ofCopies.split("\\.");

        //A OF B OF C
        for (int i = ofIds.length-1; i > 0; i--) {
            String theOfCopy = ofIds[i-1];
            String  theFieldName = name_toField(ofIds[i]);
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
            ret = getQlfNameWithoutOfCopy(ofCopyField,null) + "." + qlfNameInCopybook;
        }else {
            //Not local of and not Constant
            if(getJavaFieldToQualifiedName().get(fieldName) == null /*&&
                    !Character.isUpperCase(qlfNameInCopybook.charAt(0))*/) {
                ret = getQlfNameWithoutOfCopy(ofCopyField, null) +
                        qlfNameInCopybook.substring(beginIndex);
            }else {
                //local of
                ret = qlfNameInCopybook;
            }
        }
        return ret;
    }

    private String getQlfNameWithoutOfCopy(String fieldName, String localOf) {
        String ret;
        String qlfName = getJavaFieldToQualifiedName().get(fieldName);
        if(qlfName != null) {
            //Local field
            ret = fixAmbiguousName(fieldName,localOf,qlfName);
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
        if(exprContext == null) {
            ret = getJavaFieldToQualifiedName().get(fieldName);
            ret = fixAmbiguousName(fieldName, ofCopyField, ret);
        }
        else {
            ret = exprContext.name_qlfName(fieldName,null,ofCopyField);
        }
        return ret;
    }

    private String fixAmbiguousName(String fieldName, String ofCopyField, String multiQlfName) {
        String ret = multiQlfName;
        if(multiQlfName == null){
            debugPoint();
        }
        String[] qlfnames = multiQlfName.split("\\|");
        if(qlfnames.length > 1){
            boolean isAmbiguous = true;
            if(ofCopyField != null){
                for(String qlfName:qlfnames){
                    String[] parts = qlfName.split("\\.");
                    if(contains(ofCopyField, parts)){
                        ret = qlfName;
                        isAmbiguous = false;
                        break;
                    }
                }
            }
            if(isAmbiguous){
                ret = qlfnames[0];
                getLogger().error("Error Ambiguous filed {}:{}" , fieldName, multiQlfName);
            }
        }
        return ret;
    }

    private static boolean contains(String ofCopyField, String[] parts) {
        boolean ret = false;
        boolean isConst = Character.isUpperCase(parts[0].charAt(0));
        if(isConst)
            ofCopyField = IExprBaseContext.capitalizeFirstLetter(ofCopyField);
        for(String part:parts){
            if(part.equals(ofCopyField)){
                ret = true;
                break;
            }
        }
        return ret;
    }

    default String name_qlfUdfNameWithDim(String javaQlfName, String dimStr) {
        if(dimStr == null || dimStr.length() == 0 ||dimStr.equals("null"))
            return javaQlfName;
        String ret = null;
        String delegate = javaQlfName.substring(javaQlfName.lastIndexOf('.') + 1);
        if(getJavaFieldToQualifiedName().get(delegate) == null) {
            IExprNameContext exprContext = getExprContext(delegate, false);
            if (exprContext == null) {
                if (javaQlfName.startsWith("UNDEFINED_FIELD_"))
                    return javaQlfName;
            }
            ret = exprContext.name_qlfNameWithDim(javaQlfName, dimStr);
        }else {
            ret = name_qlfNameWithDim(javaQlfName, dimStr);
        }
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

    default String name_getFullFieldType(String fieldName) {
        String ret = getFieldToClassType().get(fieldName);
        if(ret == null){
            ret = name_getFieldType(fieldName);
        }
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
