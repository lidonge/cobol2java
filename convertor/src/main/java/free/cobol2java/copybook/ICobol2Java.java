package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.context.ExprContext;
import free.cobol2java.context.IExprBaseContext;
import free.servpp.multiexpr.IEvaluatorEnvironment;
import free.servpp.mustache.CodeFormator;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import static free.cobol2java.context.ICopybookContext.replaceImports;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public interface ICobol2Java extends ICobol2JavaBase {
    String getProgName();

    CobolPreprocessor.CobolSourceFormatEnum getFormat();

    String getEncoding();

    List<File> getCopyDirs();

    String getCblFile();

    String getRootPackageName();

    default String getProgramMustache(){
        return "/mustache/program.mustache";
    }

    default String convertAll(Map<String,Object> variables)  {
        try {
            CompilationUnit compilationUnit = getProgram();
            URL url = Cobol2JavaMustacheWriter.class.getResource(getProgramMustache());
            Cobol2JavaMustacheWriter writer = createMustacheWriter(url.toURI(),getRootPackageName(),compilationUnit.getProgramUnit());
            MustacheListenerImpl listener = createMustacheListener(url);
            convert(variables, writer, listener);
            StringBuffer sb = writer.getOutText();
            Map map = (Map) writer.getExprEvaluator().getEnvironment().getVar("importsMap");
            ExprContext exprContext = ((ExprContext) writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT));
            String progName = compilationUnit.getProgramUnit().getIdentificationDivision().getProgramIdParagraph().getName().replace("'","");
            List imports = (List) ((IEvaluatorEnvironment.MyObject)map.get(IExprBaseContext.toClassName(progName))).getValue();
            String code = replaceImports(imports,sb.toString(),true);
            return CodeFormator.formatCode(code);
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return null;
    }

    private CompilationUnit getProgram() throws IOException {
        String cblFile = getCblFile();
        File inputFile = new File(cblFile);
        URI uri = inputFile.toURI();

        CobolParserParamsImpl params = getCobolParserParams();
        return getCompilationUnit(getProgName(), getString(uri, getEncoding()), params);
    }

    private CobolParserParamsImpl getCobolParserParams() {
        List<File> copyDirs = getCopyDirs();
        String encoding = getEncoding();
        CobolPreprocessor.CobolSourceFormatEnum format = getFormat();

        CobolParserParamsImpl params = new CobolParserParamsImpl();
        if(format != null)
            params.setFormat(format);

        if(copyDirs != null)
            params.setCopyBookDirectories(copyDirs);

        if(encoding != null)
            params.setCharset(Charset.forName(encoding));
        return params;
    }
}
