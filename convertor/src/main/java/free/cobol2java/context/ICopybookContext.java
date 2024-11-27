package free.cobol2java.context;

import free.cobol2java.copybook.CopyBookManager;
import free.cobol2java.parser.CobolCompiler;
import free.cobol2java.parser.TopCompiler;
import free.servpp.logger.ILogable;
import free.servpp.multiexpr.IEvaluatorEnvironment;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface ICopybookContext extends IExprBaseContext, IExprPhysicalContext, ILogable {

    default String model_replaceImports(List imports, String code){
        return replaceImports(imports, code,true);
    }

    static String replaceImports(List imports, String code, boolean keepMark) {
        StringBuffer sbImp = new StringBuffer();
        for(Object imp: imports){
            sbImp = sbImp.append("import ").append(imp).append(";\n");
        }
        if(keepMark){
            code = code.replace("#imports#", "//#importsMark#");
            code = code.replace("//#importsMark#", "//#importsMark#\n#imports#\n");
        }

        imports.clear();
        return code.replace("#imports#", sbImp.toString());
    }

    default String model_getPackage(String modelName, String useCurrentCopybook) {
        return CopyBookManager.getDefaultManager().getPackageNameByModelName(modelName,"true".equals(useCurrentCopybook));
    }

    default IExprNameContext getExprContext(String fieldName) {
        return getExprContext(fieldName,null);
    }

    private String getFunctionType(String func){
        if(func.startsWith("Function.upperCase")||func.startsWith("Function.lowerCase"))
            return "String";
        else if(func.startsWith("Function.mod"))
            return "Integer";
        return null;
    }
    default String getClassTypeByQlfName(String fieldName, boolean fullPath) {
        if(fieldName.startsWith("Function.")){
            return getFunctionType(fieldName);
        }
        String ret = getJavaQlfFieldToFullType().get(fieldName);
        if(ret == null)
            ret = getJavaQlfFieldToSimpleType().get(fieldName);
        if(ret == null) {
            String[] paths = fieldName.split("\\.");
            String qlfCopyName = null;
            for (int i = 0; i < paths.length; i++) {
                String path = paths[i];
                if (qlfCopyName == null)
                    qlfCopyName = path;
                else
                    qlfCopyName += "." + path;
                String copyFieldName = Character.isUpperCase(qlfCopyName.charAt(0)) ? qlfCopyName
                        : getQlfNameToCopyFieldName().get(qlfCopyName);
                if (copyFieldName == null)
                    continue;
                String copybookName = IExprBaseContext.capitalizeFirstLetter(copyFieldName);
                IExprNameContext ctx = getCopybookContexts().get(copybookName);
                String nextQlfName = createQlfName(copyFieldName,paths,i);
                ret = ctx.getClassTypeByQlfName(nextQlfName,fullPath);
                break;
            }
        }else{
            if(fullPath) {
                if(!IExprBaseContext.isBaseType(ret)) {
                    String pack = model_getPackage(
                            name_toClass(fieldName.substring(0, fieldName.indexOf("."))),
                            "false");
                    if (pack != null) {
                        ret = pack + "." + ret;
                    }
                }
            }
        }
        return ret;
    }

    private String createQlfName(String copyFieldName, String[] paths, int start){
        String ret = copyFieldName;
        for(int i=start+1;i<paths.length;i++){
            ret += "."+paths[i];
        }
        return ret;
    }

    default IExprNameContext getExprContext(String fieldName, List<String> copyPath) {
        IExprNameContext value = null;
        CobolCompiler cobolCompiler = TopCompiler.currentCompiler();
        String copyBookName = getCopyBookName();
        List<String> includes = copyBookName == null ? cobolCompiler.getIncludes() :
                cobolCompiler.getIncludesOf(copyBookName);
        for (String copyName : includes) {
//            copyName = name_toClass(copyName);
            IExprNameContext ctx = getCopybookContexts().get(copyName);
            if (ctx != null) {
                boolean match = ctx.getJavaFieldToQualifiedName().get(fieldName) != null ||
                            ctx.getQlfNameToCopyFieldName().get(fieldName) != null;
                if (match) {
                    value = ctx;
                    String copyFieldQlfName = getCopyFieldNameToQlfName().get(IExprBaseContext.lowerFirstLetter(copyName));
                    if(this.getCopyBookName() != null){
                        copyFieldQlfName = copyFieldQlfName.substring(this.getCopyBookName().length()+1);
                    }
                    if(copyPath != null)
                        copyPath.add(copyFieldQlfName);
                    break;
                }
            }
        }
        if (value == null) {
            for (String copyName : includes) {
//            copyName = name_toClass(copyName);
                IExprNameContext ctx = getCopybookContexts().get(copyName);
                if(ctx == null){
                    //not a data copy book
                    continue;
                }
                value = ctx.getExprContext(fieldName, copyPath);
                if(value != null) {
                    if(copyPath != null) {
                        String copyFieldQlfName = getCopyFieldNameToQlfName().get(IExprBaseContext.lowerFirstLetter(copyName));
                        if(this.getCopyBookName() != null){
                            copyFieldQlfName = copyFieldQlfName.substring(this.getCopyBookName().length()+1);
                        }
                        copyPath.add(copyFieldQlfName);
                    }
                    break;
                }
            }
        }
        return value;
    }

    default String expr_changeAddressType(String targetVar, String operand) {
        String type = getJavaQlfFieldToSimpleType().get(targetVar);

        if (type == null) {
            String[] path = targetVar.split("\\.");
            if (path.length > 1) {
                //field in copybook
                //FIXME
                String fieldName = path[path.length - 1];
                IExprNameContext context = getExprContext(fieldName);
                String qlfName = context.getJavaFieldToQualifiedName().get(fieldName);
                type = context.getJavaQlfFieldToSimpleType().get(qlfName);
            }
        } else if (getCopybookContexts().get(type) == null) {
            //Type is an inner name
            type = getInnerClsNameToCopybookName().get(type);
        }

        if (type != null) {
            String operandField = operand.substring(operand.lastIndexOf(".") + 1);
            int idx = operandField.indexOf("[");
            if(idx != -1){
               operandField = operandField.substring(0,idx);
            }
            IExprNameContext context = getExprContext(operandField);
            if(context == null){
                getLogger().error("Error undefined field {} in copybooks.",operandField);
                return "Object";
            }
            String copyName = context.getCopyBookName();
            Map<String, String> copyBookMap = CopyBookManager.getDefaultManager().getCopyBookMap();
            String copyContent = copyBookMap.get(copyName);
            String packageName = model_getPackage(type,"true");
            String copyPackName = model_getPackage(copyName,"false");
            if(!packageName.equals(copyPackName)) {
                Map map = (Map) context.getEnvironment().getVar("importsMap");
                IEvaluatorEnvironment.MyObject myObject = (IEvaluatorEnvironment.MyObject) map.get(copyName);
                List imports = null;
                if(myObject != null) {
                    imports = (List) myObject.getValue();
                }else{
                    imports = new ArrayList();
                    map.put(copyName,new IEvaluatorEnvironment.MyObject(imports));
                }
                imports.add(packageName + "." + type);
            }
            copyContent = copyContent.replace("Object " + operandField, type + " " + operandField);
            //copyContent = copyContent.replace("Object[] " + operandField, type + "[] " + operandField);
            copyBookMap.put(copyName, copyContent);
        }
        return type;
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
