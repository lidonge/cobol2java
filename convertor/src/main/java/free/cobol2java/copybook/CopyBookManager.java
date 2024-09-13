package free.cobol2java.copybook;

import free.cobol2java.ExprContext;
import free.cobol2java.ICobolConvertor;
import free.servpp.mustache.CodeFormator;
import free.servpp.mustache.handler.MustacheWriter;
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
public class CopyBookManager implements ICobol2JavaBase {
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
    ExprContext globalExprContext = new ExprContext();

    public ExprContext getGlobalFunc() {
        return globalExprContext;
    }

    public void init(ICobolConvertor cobolConvertor) {
        this.cobolConvertor = cobolConvertor;
    }

    public void loadCopyBook(File copyBook,
                             CobolParserParams params,
                             String copyText) throws URISyntaxException, IOException, CopybookException {
        String name = copyBook.getName();
        name = globalExprContext.name_toClass(name);
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
            Map<String,Object> varables = new HashMap<>();
            varables.put(LOCAL_CONTEXT, globalExprContext);
            if(name.endsWith("CONST"))
                varables.put("IsConstantCopybook","IsConstantCopybook");

            MustacheWriter writer = convertProgram(varables,compilationUnit.getProgramUnit(),"/mustache/copybook.mustache","com.dcits");

            String prog = CodeFormator.formatCode(writer.getOutText().toString());
            copyBookMap.put(name,prog);
            Map<String,Object> copyBookCls = (Map<String, Object>) writer.getExprEvaluator().getEnvironment().getVar("innerMap");
            for(Map.Entry<String,Object> entry :copyBookCls.entrySet()){
                copyBookMap.put(entry.getKey(),entry.getValue().toString());
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
