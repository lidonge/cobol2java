package free.cobol2java.context;

import free.cobol2java.parser.CobolCompiler;
import free.cobol2java.parser.TopCompiler;

import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface ICopybookContext extends IExprBaseContext, IExprPhysicalContext{
    default IExprNameContext getOfCopyContext(String ofCopyField) {
        String qlfName = getJavaFieldToQualifiedName().get(ofCopyField);
        //Field leve is 75 or is defined in a copybook
        if(qlfName == null)
            qlfName = ofCopyField;

        String ofCopyCls = getJavaQlfFieldToType().get(qlfName);
        String realCopyCls = null;
        IExprNameContext exprContext = null;
        if(ofCopyCls == null){
            //of copy is not defined in main cobol
            exprContext = getExprContext(ofCopyField, false);
        }else{
            realCopyCls = getInnerClsNameToCopybookName().get(ofCopyCls);
            exprContext = getCopybookContexts().get(realCopyCls != null ? realCopyCls : ofCopyCls);
        }
        return exprContext;
    }

    default IExprNameContext getExprContext(String fieldName, boolean isMiddleName) {
        IExprNameContext value = null;
        CobolCompiler cobolCompiler = TopCompiler.currentCompiler();
        String copyBookName = getCopyBookName();
        List<String> includes =  copyBookName == null ?cobolCompiler.getIncludes() :
                cobolCompiler.getIncludesOf(copyBookName);
        for(String copyName:includes){
//            copyName = name_toClass(copyName);
            IExprNameContext ctx = getCopybookContexts().get(copyName);
            if(ctx != null){
                boolean match = false;
                if(isMiddleName) {
                    match = ctx.getCopyFieldNameToJavaFileName().get(fieldName) != null;
                }else{
                    match = ctx.getJavaFieldToQualifiedName().get(fieldName) != null ||
                            ctx.getJavaFieldNameToCopyFieldName().get(fieldName) != null;
                }
                if(match) {
                    value = ctx;
                    break;
                }
            }
        }
        return value;
    }

//
//    default String copybook(String innerFieldName, String copyFieldName){
//        IExprNameContext ctx = getCopybookContexts().get(copyFieldName);
//
//        for(Map.Entry<String,String> entry:ctx.getJavaFieldToQualifiedName().entrySet()){
//            String value = entry.getValue();
//            String qlfName = innerFieldName + value.substring(value.indexOf('.'));
//            getJavaFieldToQualifiedName().put(entry.getKey(), qlfName);
//            makeQlfNameAllLevel(qlfName);
//        }
//
//        for(Map.Entry<String,Number> entry:ctx.getJavaFieldNameToDim().entrySet()){
//            Number value = entry.getValue();
//            getJavaFieldNameToDim().put(entry.getKey(), value);
//        }
//        return "";
//    }

}
