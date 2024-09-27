package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.ExprContext;
import free.cobol2java.ICobolConvertor;
import free.servpp.mustache.CodeFormator;
import free.servpp.mustache.ILogable;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.params.CobolParserParams;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
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
        name = ExprContext.toClassName(name);

        String copyBookJavaText = copyBookMap.get(name);
        if(copyBookJavaText == null) {
            URL url = CopyBookManager.class.getResource(modelTemplate);
            String sText = getString(url.toURI());
            sText = sText.replace("COPY-BOOK-NAME", name);
            sText += "\n" + copyText;
            CompilationUnit compilationUnit;
            try {
                compilationUnit = getCompilationUnit(name, sText, params);
            }catch (Throwable t){
                throw new CopybookException(t);
            }
            getLogger().info("Open Data copybook :{}", name);
            Map<String,Object> variables = new HashMap<>();
//            variables.put(COPYBOOK_CONTEXT, exprContextMap);
            if(name.endsWith("const"))
                variables.put("IsConstantCopybook","IsConstantCopybook");

            Cobol2JavaMustacheWriter writer = createMustacheWriter("com.dcits",compilationUnit.getProgramUnit());
            for (Map.Entry<String,ExprContext> entry:exprContextMap.entrySet()){
                entry.getValue().setEnvironment(writer.getExprEvaluator().getEnvironment());
            }
            MustacheListenerImpl listener = createMustacheListener("/mustache/copybook.mustache");
            convert(variables, writer, listener);


            String prog = CodeFormator.formatCode(writer.getOutText().toString());
            if(prog.trim().length() != 0)
                copyBookMap.put(name,prog);
            exprContextMap.put(name, (ExprContext) writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT));
            Map<String,Object> copyBookCls = (Map<String, Object>) writer.getExprEvaluator().getEnvironment().getVar("innerMap");
            for(Map.Entry<String,Object> entry :copyBookCls.entrySet()){
                String string = entry.getValue().toString();
                if(string.trim().length() != 0)
                    copyBookMap.put(entry.getKey(), string);
            }

//            System.out.println(prog);
        }
    }

    public boolean isCopybookManage() {
        return cobolConvertor.isCopybookManage();
    }

    public Map<String, String> getCopyBookMap() {
        return copyBookMap;
    }
}
