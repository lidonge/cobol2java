package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
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
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

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
            Cobol2JavaMustacheWriter writer = createMustacheWriter(getRootPackageName(),compilationUnit.getProgramUnit());
            MustacheListenerImpl listener = createMustacheListener(getProgramMustache());
            convert(variables, writer, listener);
            StringBuffer sb = writer.getOutText();

            return CodeFormator.formatCode(sb.toString());
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
        return getCompilationUnit(getProgName(), getString(uri), params);
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
