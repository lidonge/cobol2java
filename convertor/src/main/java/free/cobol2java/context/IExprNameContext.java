package free.cobol2java.context;


import free.cobol2java.util.CobolConstant;
import free.servpp.logger.ILogable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprNameContext extends ILogable, IExprEnvContext, IExprPhysicalContext,
        IExprBaseContext, ICopybookContext, IExprCtxHandler,
        IExprDimensionContext {
    record ContextAndQlfName(String qlfName, IExprNameContext context) {
    }

    default boolean name_isConst(String name) {
        return CobolConstant.isConstant(name);
    }

    default String name_qlfName(String fieldName, String ofCopies) {
        String ret = null;
        boolean isInCopy = ofCopies != null && ofCopies.length() != 0 && !"null".equals(ofCopies);
        if (isInCopy) {
            ret = getQlfNameWithOfCopies(fieldName, ofCopies);
        }else {
            ret = getQlfNameInMain(fieldName);
        }
        return ret;
    }
    private String getQlfNameWithOfCopies(String fieldName, String ofCopies) {
        String ret = null;
        String[] ofIds = (fieldName + "." + ofCopies).split("\\.");
        //A OF B OF C = C.B.A =
        // qlfName include C and end with a fieldName of a copybook BOOK1 +
        // qlfName include B start with BOOK1(should be remove) and end with fieldName of BOOk2 +
        // qlfName start with BOOK2(should be remove) and end with A
        ContextAndQlfName prev = new ContextAndQlfName(null,this);
        for (int i = ofIds.length - 1; i > 0; i--) {
            String field = ofIds[i - 1];
            String ofField = ofIds[i];
            //get name_Field_In_ofField(-ofField)
            ContextAndQlfName caq = prev.context.getQlfNameOf(field, ofField);
            String qlfName = caq.qlfName;
            if(qlfName.indexOf("|") != -1){
                qlfName = getQlfNameFromMultiNames(ofField,qlfName);
            }
            if (ret == null) {
                //nameCInMain
                ret = qlfName;
            } else {
                if(prev.context == caq.context){
                    ret = qlfName;
                }else {
                    qlfName = qlfName.substring(qlfName.indexOf(ofField) + ofField.length());
                    ret = ret + qlfName;
                }
            }
            prev = caq;
        }
        return ret;
    }

    //A of B = (-copyName)...A
    private ContextAndQlfName getQlfNameOf(String fieldA, String fieldB) {
        String qlfName = null;
        IExprNameContext exprContext = null;
        //to find fieldA in the copybook that fieldB in.
        //First to find if fieldB is defined in main cbl
        String fieldBQlfName = getJavaFieldToQualifiedName().get(fieldB);
        if (fieldBQlfName != null) {
            //found the qlfName of fieldB in main
            //Test if the fieldB is a copybook, FIXME should put with qlfName
            String copyFieldName = getQlfNameToCopyFieldName().get(fieldBQlfName);
            if (copyFieldName == null) {
                //fieldB not a copybook
                String multiQlfName = getJavaFieldToQualifiedName().get(fieldA);
                qlfName = getQlfNameFromMultiNames(fieldB, multiQlfName);
            } else {
                //fieldB is a copybook
                String copyFieldQlfName = getJavaFieldToQualifiedName().get(copyFieldName);
//                String copybookName = getJavaQlfFieldToSimpleType().get(copyFieldQlfName);
                String copybookName = name_toClass(copyFieldName);
                exprContext = getCopybookContexts().get(copybookName);
                String fieldAQlfNameInCopy = exprContext.getQlfNameInMain(fieldA);
                //FIXME field defined in filler
                qlfName = fieldBQlfName +
                        (fieldAQlfNameInCopy.startsWith("UNDEFINED_") ? fieldAQlfNameInCopy: fieldAQlfNameInCopy.substring(fieldAQlfNameInCopy.indexOf(".")));
            }
        } else {
            //fieldB not defined in main, to find qlfName of fieldB in copybook
            //004-COPY-OF-ONE. DISPLAY CB-SIMP2-B OF CP-SIMP2.
            List<String> copyPath = new ArrayList<>();
            exprContext =getExprContext(fieldB,copyPath);
            String fieldAQlfNameInCopy = exprContext.getQlfNameInMain(fieldA);
            qlfName = changeCopyFieldNameToFieldName(fieldAQlfNameInCopy,copyPath);
        }
        return new ContextAndQlfName(qlfName,exprContext);
    }

    private String getQlfNameInMain(String fieldA) {
        String ret = null;
        //to find fieldA in main cbl
        String qlfFieldA = getJavaFieldToQualifiedName().get(fieldA);
        if (qlfFieldA != null) {
            //the field defined in the main cbl
            if (qlfFieldA.indexOf("|") != -1) {
                getLogger(IExprNameContext.class).error("Error Ambiguous name {}:{}", fieldA, ret);
            }
            ret = qlfFieldA;
        } else {
            //the fieldA is in copyfile not specified, get the context that fieldA exists
            List<String> copyPath = new ArrayList<>();
            IExprNameContext exprContext = getExprContext(fieldA,copyPath);
            //get the qlfName of fieldA in its context.
            //FIXME the field defined in filler
            if(exprContext == null){
                ret = "UNDEFINED_"+fieldA;
                getLogger(IExprNameContext.class).error("Error can not find field {} defined in copybook {}.", fieldA,this.getCopyBookName());
            }else {
                String qlfNameInCopy = exprContext.getJavaFieldToQualifiedName().get(fieldA);
                ret = changeCopyFieldNameToFieldName(qlfNameInCopy,copyPath);
            }
        }
        return ret;
    }

    private String changeCopyFieldNameToFieldName(String qlfNameInCopy, List<String> copyPath) {
        String ret;
        if(Character.isUpperCase(qlfNameInCopy.charAt(0))){
            //Constant copybook
            ret = qlfNameInCopy;
        }else {
            //the copybook should be included by main cbl
            int index = qlfNameInCopy.indexOf(".");
            String copyFileName = qlfNameInCopy.substring(0, index);
            //find the field defined in main cbl, it should only one instance of the class
            String javaFieldNameOfCopy = getCopyFieldNameToQlfName().get(copyFileName);
            if(javaFieldNameOfCopy == null) {
                for (String copyName : copyPath) {
                    if (javaFieldNameOfCopy == null)
                        javaFieldNameOfCopy = copyName;
                    else
                        javaFieldNameOfCopy = copyName + "." + javaFieldNameOfCopy;
                }
            }
            ret = javaFieldNameOfCopy + qlfNameInCopy.substring(index);
        }
        return ret;
    }

    private String getQlfNameFromMultiNames(String ofCopyField, String multiQlfName) {
        String ret = null;
        String[] qlfnames = multiQlfName.split("\\|");
        for (String qlfName : qlfnames) {
            String[] parts = qlfName.split("\\.");
            if (contains(ofCopyField, parts)) {
                if(ret == null)
                    ret = qlfName;
                else
                    ret += "|" + qlfName;
            }
        }
        return ret;
    }

    private boolean contains(String ofCopyField, String[] parts) {
        boolean ret = false;
        boolean isConst = Character.isUpperCase(parts[0].charAt(0));
        if (isConst)
            ofCopyField = IExprBaseContext.capitalizeFirstLetter(ofCopyField);
        for (String part : parts) {
            if (part.equals(ofCopyField)) {
                ret = true;
                break;
            }
        }
        return ret;
    }


    //===================================
    default String name_qlfNameWithDim(String theJavaQlfName, String dimStr) {
        String ret = null;
        if(theJavaQlfName.indexOf("UNDEFINED_")!=-1){
            //FIXME the cobol error
            ret = theJavaQlfName;
        }else {
            if (dimStr == null) {
                ret = theJavaQlfName;
            } else {
                if (dimStr.indexOf(":") != -1) {
                    String dim = dimStr.replace(":", ",");
                    ret = "Util.subvalue(" + theJavaQlfName + "," + dim + ")";
                } else
                    ret = addDimToQlfName(theJavaQlfName, dimStr);
            }
        }
//        ret = nestedQualifiedName(ret);

        return ret;
    }

    default String name_getFullFieldType(String fieldName) {
        String ret = getJavaQlfFieldToFullType().get(fieldName);
        if (ret == null) {
            ret = _getFieldType(fieldName,true);
        }
        return ret;
    }

    default String name_getFieldType(String fieldName) {
        return _getFieldType(fieldName,false);
    }
    default String _getFieldType(String fieldName, boolean fullPath) {
        String ret = getJavaQlfFieldToSimpleType().get(fieldName);
        if (ret != null && getInnerClsNameToCopybookName().get(ret) != null) {
            ret = getInnerClsNameToCopybookName().get(ret);
        }
        if (ret == null) {
            ret = name_getFieldClsType(fieldName);
            if (ret == null) {
                int index = fieldName.lastIndexOf('.');
                String shortName = fieldName;
                if (index != -1) {
                    shortName = fieldName.substring(index + 1);
                }
                IExprNameContext exprContext = getExprContext(shortName);
                if (exprContext != null) {
                    ret = exprContext.name_getFieldType(exprContext.name_qlfName(shortName, null));
                    if(fullPath) {
                        if(!IExprBaseContext.isBaseType(ret)) {
                            String pack = exprContext.model_getPackage(
                                    name_toClass(fieldName.substring(0, fieldName.indexOf("."))),
                                    "false");
                            if (pack != null) {
                                ret = pack + "." + ret;
                            }
                        }
                    }
                }
            }
        }
        return ret;
    }

    default String name_getFieldClsType(String fieldName) {
        return getJavaQlfFieldToFullType().get(fieldName);
    }
}
