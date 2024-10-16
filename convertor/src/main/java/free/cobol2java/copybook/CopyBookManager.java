package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.context.ExprContext;
import free.cobol2java.ICobolConvertor;
import free.cobol2java.context.IExprBaseContext;
import free.cobol2java.parser.TopCompiler;
import free.servpp.logger.ILogable;
import free.servpp.multiexpr.IEvaluatorEnvironment;
import free.servpp.mustache.CodeFormator;
import free.servpp.mustache.handler.MustacheListenerImpl;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.params.CobolParserParams;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public class CopyBookManager implements ICobol2JavaBase , ILogable {
    private static final String modelTemplate = "/ModelCopyBook.cbl";
    private static CopyBookManager defaultManager = new CopyBookManager();

    public static void initDefaultManager(ICobolConvertor cobolConvertor) {
        defaultManager.init(cobolConvertor);
    }

    public static CopyBookManager getDefaultManager() {
        return defaultManager;
    }

    public static void setDefaultManager(CopyBookManager defaultManager) {
        CopyBookManager.defaultManager = defaultManager;
    }

    private ICobolConvertor cobolConvertor;
    Map<String,String> copyBookMap = new HashMap<>();
    Map<String,ExprContext> exprContextMap = new HashMap<>();
//    Map<String,String> subNameToCopybook = new HashMap<>();
    Map<String,String> classNameToPackageName = new HashMap<>();
//    ExprContext globalExprContext = new ExprContext();

    public Map<String,ExprContext> getGlobalFunc() {
        return exprContextMap;
    }

    public void init(ICobolConvertor cobolConvertor) {
        this.cobolConvertor = cobolConvertor;
    }

    public void loadCopyBook(File copyBook,
                             CobolParserParams params,
                             String copyText) throws URISyntaxException, IOException, CopybookException {
        String name = copyBook.getName();
        name = IExprBaseContext.toClassName(name);

        String copyBookJavaText = copyBookMap.get(name);
        if(copyBookJavaText == null) {
            getLogger().info("Loading copybook:{}",copyBook.getName());
            URL url = CopyBookManager.class.getResource(modelTemplate);
            String sText = getString(url.toURI(), cobolConvertor.getEncoding());
            sText = sText.replace("COPY-BOOK-NAME", name);
            sText += "\n" + copyText;
            CompilationUnit compilationUnit;
            try {
                compilationUnit = getCompilationUnit(name, sText, params);
            }catch (Throwable t){
                throw new CopybookException(t);
            }
            Map<String,Object> variables = new HashMap<>();
//            variables.put(COPYBOOK_CONTEXT, exprContextMap);
            if(name.endsWith("const"))
                variables.put("IsConstantCopybook","IsConstantCopybook");

            Cobol2JavaMustacheWriter writer = createMustacheWriter(url.toURI(),cobolConvertor.getRootPackageName(),
                    compilationUnit.getProgramUnit());

            ExprContext exprContext = (ExprContext) writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT);
            exprContext.setCopyBookName(name);
            exprContext.setCopyBookPath(copyBook.getAbsolutePath());
            exprContextMap.put(name, exprContext);
            String packageName = cobolConvertor.getRootPackageName() + ".models"+
                    getRelativePath(name).replace(File.separator,".");
            packageName = ICobolConvertor.checkPackageName(packageName);
            classNameToPackageName.put(name,packageName);

            MustacheListenerImpl listener = createMustacheListener("/mustache/copybook.mustache");
            convert(variables, writer, listener);

            String prog = CodeFormator.formatCode(writer.getOutText().toString());
            if(prog.trim().length() != 0) {
                copyBookMap.put(name, prog);
            }
            Map<String,Object> copyBookCls = (Map<String, Object>) writer.getExprEvaluator().getEnvironment().getVar("innerMap");
            for(Map.Entry<String,Object> entry :copyBookCls.entrySet()){
                String string = entry.getValue().toString();
                if(string.trim().length() != 0) {
                    copyBookMap.put(entry.getKey(), string);
                    classNameToPackageName.put(entry.getKey(),packageName);
                }
            }

//            getLogger().info("Copybook:{} Loaded. ",copyBook.getName());
        }
    }
    public String getPackageNameByModelName(String modelName, boolean useCurrentCopybook){
        String ret = classNameToPackageName.get(modelName);
        if(useCurrentCopybook && ret == null){
            String curCopybook = TopCompiler.currentCompiler().currentCopybook();
            ret = classNameToPackageName.get(curCopybook);
        }
        return ret;
    }

    public void writeCopyBook() {
        for(String key: copyBookMap.keySet()){
            String text = copyBookMap.get(key);
            if(!"WRITED".equals(text)){
                String packageName = classNameToPackageName.get(key);
                String relativePath = packageName.replace(".",File.separator);
                File outFile = new File(cobolConvertor.getTargetPath(),relativePath);
                outFile = new File(outFile,key + ".java");
                String outputFilePath =outFile.getAbsolutePath();
                ExprContext exprContext = exprContextMap.get(key);
                if(exprContext != null) {
                    Map map = (Map) exprContext.getEnvironment().getVar("importsMap");
                    IEvaluatorEnvironment.MyObject myObject = (IEvaluatorEnvironment.MyObject) map.get(key);
                    List imports = null;
                    if(myObject != null) {
                        imports = (List) myObject.getValue();
                        text = exprContext.replaceImports( imports, text,true);
                        copyBookMap.put(key,text);
                    }

                }
                String content = CodeFormator.formatCode(text+"");
                cobolConvertor.writeToFile(outputFilePath, content);
                if(content.indexOf("public Object ") == -1 /*|| content.indexOf("public Object[] ") == -1*/)
                    copyBookMap.put(key,"WRITED");
            }
        }
    }

    private String getRelativePath(String copyName) {
        ExprContext exprContext = exprContextMap.get(copyName);
        String fullPath = exprContext.getCopyBookPath();
        String srcRoot = cobolConvertor.getSourcePath();
        fullPath = new File(fullPath).getParent();
        srcRoot = new File(srcRoot).getAbsolutePath();
        String relativePath = "";
        if(fullPath.indexOf(srcRoot) != -1) {
            relativePath = fullPath.substring(srcRoot.length());
        }
        return relativePath;
    }

    public boolean isCopybookManage() {
        return cobolConvertor.isCopybookManage();
    }

    public Map<String, String> getCopyBookMap() {
        return copyBookMap;
    }
}
