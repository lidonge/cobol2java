package free.cobol2java;

import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import free.servpp.mustache.model.BaseSection;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;

/**
 * @author lidong@date 2024-08-30@version 1.0
 */
public class Cobol2Java {
    private String cblFile;
    private String progName;
    private String encoding;
    private String packageName;
    private CobolPreprocessor.CobolSourceFormatEnum format;

    public Cobol2Java(String cblFile, String progName, String packageName) {
        this(cblFile,progName,packageName,CobolPreprocessor.CobolSourceFormatEnum.TANDEM,null);
    }

    public Cobol2Java(String cblFile, String progName, String packageName, CobolPreprocessor.CobolSourceFormatEnum format, String encoding) {
        this.cblFile = cblFile;
        this.progName = progName;
        this.encoding = encoding;
        this.packageName = packageName;
        this.format = format;
    }

    public String convertAll()  {
        try {
            CompilationUnit compilationUnit = null;
            compilationUnit = getProgram();
            ProgramUnit programUnit = compilationUnit.getProgramUnit();
            String prog = convertProgram(programUnit);
            return prog;
        } catch (URISyntaxException|IOException e) {
            e.printStackTrace();
        }
//            new ObjectTreePrinter().printObjectTree(compilationUnit);
//        new ObjectTreePrinter().printObjectTree(programUnit);
        return null;
    }

    private CompilationUnit getProgram() throws IOException {
        File inputFile = new File(cblFile);

        CobolParserRunnerImpl cobolParserRunner = new CobolParserRunnerImpl() {
            public Program analyzeFile(final File cobolFile, final CobolPreprocessor.CobolSourceFormatEnum format) throws IOException {
                final CobolParserParams params = createDefaultParams(format, cobolFile);
                if(encoding != null)
                    params.setCharset(Charset.forName(encoding));
                return analyzeFile(cobolFile, params);
            }
        };
        Program program = cobolParserRunner.analyzeFile(inputFile, format);
        CompilationUnit compilationUnit = program.getCompilationUnit(progName);
        return compilationUnit;
    }

    String convertProgram(ProgramUnit programUnit) throws IOException, URISyntaxException {
        URL url = Cobol2JavaMustacheWriter.class.getResource(File.separator + "program.mustache");
        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
        MustacheListenerImpl impl = mustacheCompiler.compile();

        MustacheWriter writer = getMustacheWriter(programUnit);
        writer.write(impl.getTemplate(), BaseSection.SectionType.Normal);
        StringBuffer sb = writer.getOutText();

        return CodeFormator.formatCode(sb.toString());
    }


    private MustacheWriter getMustacheWriter(Object root) {
        return new Cobol2JavaMustacheWriter(root, packageName);
    }
}
