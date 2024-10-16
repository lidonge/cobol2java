package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.parser.ExtCobolParserRunnerImpl;
import free.cobol2java.context.ExprContext;
import free.cobol2java.util.IUrlLoader;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.model.BaseSection;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public interface ICobol2JavaBase extends IUrlLoader {
    String LOCAL_CONTEXT = "__System_Context";
//    String COPYBOOK_CONTEXT = "__Copybook_Context";
    String OR_MAPPING_CONTEXT = "__OR_Mapping_Context";
    String COBOL_CONVERTOR = "COBOL_CONVERTOR";

    default CompilationUnit getCompilationUnit(String progName, String sText, CobolParserParams params) throws IOException {
        CobolParserRunnerImpl cobolParserRunner = new ExtCobolParserRunnerImpl();
        Program program = cobolParserRunner.analyzeCode(sText, progName,params);
        CompilationUnit compilationUnit = program.getCompilationUnit(progName);
        return compilationUnit;
    }

    default MustacheListenerImpl createMustacheListener(String mustache) throws URISyntaxException, IOException {
        URL url = Cobol2JavaMustacheWriter.class.getResource(mustache);
        return createMustacheListener(url);
    }

    default MustacheListenerImpl createMustacheListener(URL url) throws URISyntaxException, IOException {
        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
        mustacheCompiler.compileAntlr4(null);
        MustacheListenerImpl listener = new MustacheListenerImpl(url.toURI());
        mustacheCompiler.workListener(listener);
        return listener;
    }

    default void convert(Map<String, Object> variables, Cobol2JavaMustacheWriter writer, MustacheListenerImpl impl) {
        writer.getExprEvaluator().getEnvironment().setVar(COBOL_CONVERTOR, CobolConfig.getCobolConvertor());
        for(Map.Entry<String,Object> entry: variables.entrySet())
            writer.getExprEvaluator().getEnvironment().setVar(entry.getKey(), entry.getValue());

        if(variables.get(OR_MAPPING_CONTEXT) != null){
            ((ExprContext) writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT)).
                    setOrMappingContext((ExprContext) variables.get(OR_MAPPING_CONTEXT));
        }
        writer.write(impl.getTemplate(), BaseSection.SectionType.Normal);
    }

    default Cobol2JavaMustacheWriter createMustacheWriter(URI mustacheFile, String packageName, Object root) {
        return new Cobol2JavaMustacheWriter(mustacheFile,root, packageName,false);
    }

}
