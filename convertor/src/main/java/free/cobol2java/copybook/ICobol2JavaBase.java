package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.ICobolConvertor;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.parser.ExtCobolParserRunnerImpl;
import free.cobol2java.ExprContext;
import free.cobol2java.util.IUrlLoader;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import free.servpp.mustache.model.BaseSection;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public interface ICobol2JavaBase extends IUrlLoader {
    String LOCAL_CONTEXT = "__System_Context";
    String COPYBOOK_CONTEXT = "__Copybook_Context";
    String OR_MAPPING_CONTEXT = "__OR_Mapping_Context";
    String COBOL_CONVERTOR = "COBOL_CONVERTOR";

    default CompilationUnit getCompilationUnit(String progName, String sText, CobolParserParams params) throws IOException {
        CobolParserRunnerImpl cobolParserRunner = new ExtCobolParserRunnerImpl();
        Program program = cobolParserRunner.analyzeCode(sText, progName,params);
        CompilationUnit compilationUnit = program.getCompilationUnit(progName);
        return compilationUnit;
    }

    default MustacheWriter convertProgram(Map<String, Object> variables,
                                          ProgramUnit programUnit,
                                          String mustache,
                                          String packageName) throws IOException, URISyntaxException {
        URL url = Cobol2JavaMustacheWriter.class.getResource(mustache);
        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
        MustacheListenerImpl impl = mustacheCompiler.compile();

        Cobol2JavaMustacheWriter writer = getMustacheWriter(packageName, programUnit);
        writer.getExprEvaluator().getEnvironment().setVar(COBOL_CONVERTOR, CobolConfig.getCobolConvertor());
        for(Map.Entry<String,Object> entry:variables.entrySet())
            writer.getExprEvaluator().getEnvironment().setVar(entry.getKey(), entry.getValue());
        if(variables.get(COPYBOOK_CONTEXT) != null){
            ((ExprContext)writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT)).setCopybookContext((ExprContext) variables.get(COPYBOOK_CONTEXT));
        }
        if(variables.get(OR_MAPPING_CONTEXT) != null){
            ((ExprContext)writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT)).setCopybookContext((ExprContext) variables.get(OR_MAPPING_CONTEXT));
        }
        writer.write(impl.getTemplate(), BaseSection.SectionType.Normal);
        return writer;
    }

    private Cobol2JavaMustacheWriter getMustacheWriter(String packageName, Object root) {
        return new Cobol2JavaMustacheWriter(root, packageName,false);
    }

}