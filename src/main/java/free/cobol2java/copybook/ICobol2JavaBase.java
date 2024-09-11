package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.CodeFormator;
import free.cobol2java.parser.ExtCobolParserRunnerImpl;
import free.cobol2java.util.Func;
import free.cobol2java.util.IUrlLoader;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import free.servpp.mustache.model.BaseSection;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
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
    String SYSTEM_FUNCTION = "__System_Function";
    String GLOBAL_FUNCTION = "__Global_Function";
    default CompilationUnit getCompilationUnit(String progName, String sText, CobolParserParams params) throws IOException {
        CobolParserRunnerImpl cobolParserRunner = new ExtCobolParserRunnerImpl();
        Program program = cobolParserRunner.analyzeCode(sText, progName,params);
        CompilationUnit compilationUnit = program.getCompilationUnit(progName);
        return compilationUnit;
    }

    default MustacheWriter convertProgram(Map<String,Object> variables, ProgramUnit programUnit, String mustache, String packageName) throws IOException, URISyntaxException {
        URL url = Cobol2JavaMustacheWriter.class.getResource(mustache);
        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
        MustacheListenerImpl impl = mustacheCompiler.compile();

        MustacheWriter writer = getMustacheWriter(packageName, programUnit);
        for(Map.Entry<String,Object> entry:variables.entrySet())
            writer.getExprEvaluator().getEnvironment().setVar(entry.getKey(), entry.getValue());
        if(variables.get(GLOBAL_FUNCTION) != null){
            ((Func)writer.getExprEvaluator().getEnvironment().getVar(SYSTEM_FUNCTION)).setGlobalFunc((Func) variables.get(GLOBAL_FUNCTION));
        }
        writer.write(impl.getTemplate(), BaseSection.SectionType.Normal);
        return writer;
    }

    default MustacheWriter getMustacheWriter(String packageName, Object root) {
        return new Cobol2JavaMustacheWriter(root, packageName,false);
    }

}
