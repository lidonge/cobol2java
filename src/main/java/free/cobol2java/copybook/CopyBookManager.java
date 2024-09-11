package free.cobol2java.copybook;

import free.cobol2java.BaseConvertor;
import free.cobol2java.CodeFormator;
import free.cobol2java.util.Func;
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

    public static void initDefaultManager(List<File> copyDirs, String rootPackageName, String format, String encoding, boolean copybookManage) {
        defaultManager.init(copyDirs, rootPackageName,format,encoding, copybookManage);
    }

    public static CopyBookManager getDefaultManager() {
        return defaultManager;
    }

    public static void setDefaultManager(CopyBookManager defaultManager) {
        CopyBookManager.defaultManager = defaultManager;
    }

    private List<File> copyDirs;
    private String rootPackageName;
    private String format;
    private String encoding;
    private boolean copybookManage = false;
    Map<String,String> copyBookMap = new HashMap<>();
    Func globalFunc = new Func();

    public Func getGlobalFunc() {
        return globalFunc;
    }

    public void init(List<File> copyDirs, String rootPackageName, String format, String encoding, boolean copybookManage) {
        setCopyDirs(copyDirs);
        setRootPackageName(rootPackageName);
        setFormat(format);
        setEncoding(encoding);
        setCopybookManage(copybookManage);
    }

    public void loadCopyBook(File copyBook, CobolParserParams params, String copyText) throws URISyntaxException, IOException, CopybookException {
        String name = copyBook.getName();
        name = globalFunc.name_toClass(name);
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
            varables.put(SYSTEM_FUNCTION, globalFunc);
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
        return copybookManage;
    }

    public void setCopybookManage(boolean copybookManage) {
        this.copybookManage = copybookManage;
    }

    public List<File> getCopyDirs() {
        return copyDirs;
    }

    public void setCopyDirs(List<File> copyDirs) {
        this.copyDirs = copyDirs;
    }

    public String getRootPackageName() {
        return rootPackageName;
    }

    public void setRootPackageName(String rootPackageName) {
        this.rootPackageName = rootPackageName;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public String getEncoding() {
        return encoding;
    }

    public void setEncoding(String encoding) {
        this.encoding = encoding;
    }

    public Map<String, String> getCopyBookMap() {
        return copyBookMap;
    }
}
