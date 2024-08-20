package free.cobol2java;

import free.cobol2java.util.Func;
import free.mustache.MustacheCompiler;
import free.mustache.TestRecursion;
import free.mustache.handler.DefaultEnvironment;
import free.mustache.handler.IPartialFileHandler;
import free.mustache.handler.MustacheListenerImpl;
import free.mustache.handler.MustacheWriter;
import free.mustache.model.BaseSection;
import free.mustache.model.Template;
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
import java.util.ArrayList;

/**
 * @author lidong@date 2024-07-30@version 1.0
 */
public class Test {
    static String cblDir = "/Users/lidong/gitspace/cobol2java/src/main/COBOL/";
    static String cobolFile = cblDir + "Example.cbl";

    static String acceptCbl = "/Users/lidong/gitspace/cobol2java/src/main/COBOL/accept.cbl";
    static String initCbl = "/Users/lidong/gitspace/cobol2java/src/main/COBOL/init.cbl";
    static String GSA01060Cbl = "/Users/lidong/gitspace/cobol2java/src/main/COBOL/GSA01060.cbl";
    static String mustacheDataFile = "/Users/lidong/gitspace/cobol2java/src/main/resources/model.mustache";
    static String mustacheProgramFile = "/Users/lidong/gitspace/cobol2java/src/main/resources/program.mustache";

    public static void main(String[] args) throws IOException {
//        new ObjectTreePrinter().printObjectTree(compilationUnit);
        convertAll(cblDir + "Table.cbl", "Table");
        if (true) {
            convertAll(cblDir + "Data.cbl", "Data");
            convertAll(cblDir + "Accept.cbl", "Accept");
            convertAll(cblDir + "Init.cbl", "Init");
            convertAll(cblDir + "Move.cbl", "Move");
            convertAll(cblDir + "Add.cbl", "Add");
            convertAll(cblDir + "Subtract.cbl", "Subtract");
            convertAll(cblDir + "Multiply.cbl", "Multiply");
            convertAll(cblDir + "Divide.cbl", "Divide");
            convertAll(cblDir + "Compute.cbl", "Compute");
            convertAll(cblDir + "Table.cbl", "Table");
            convertAll(cblDir + "Perform.cbl", "Perform");
            convertAll(cblDir + "Example.cbl", "EXAMPLE");
//            convertAll(cblDir + "GSA01060NC.cbl", "GSA01060NC",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312");
        }
    }

    private static void convertAll(String cblFile, String progName) throws IOException {
        CobolPreprocessor.CobolSourceFormatEnum format = CobolPreprocessor.CobolSourceFormatEnum.TANDEM;
        convertAll(cblFile, progName, format,null);
    }

    private static void convertAll(String cblFile, String progName, CobolPreprocessor.CobolSourceFormatEnum format, String encoding) throws IOException {
        CompilationUnit compilationUnit = getProgram(cblFile, progName, format, encoding);
//            new ObjectTreePrinter().printObjectTree(compilationUnit);
        ProgramUnit programUnit = compilationUnit.getProgramUnit();
        String prog = convertProgram(programUnit);
        System.out.println(prog);
    }

    private static CompilationUnit getProgram(String cblFile, String compUnit, CobolPreprocessor.CobolSourceFormatEnum format, String encoding) throws IOException {
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
        CompilationUnit compilationUnit = program.getCompilationUnit(compUnit);
        return compilationUnit;
    }

    static String convertProgram(ProgramUnit programUnit) throws IOException {
        MustacheCompiler mustacheCompiler = new MustacheCompiler(new File(mustacheProgramFile));
        MustacheListenerImpl impl = mustacheCompiler.compile();

        MustacheWriter writer = getMustacheWriter();
        StringBuffer sb = new StringBuffer();
        writer.write(sb, new ArrayList<>(), programUnit, impl.getTemplate(), BaseSection.SectionType.Normal);

        return CodeFormator.formatCode(sb.toString());
    }

    static String convertData(ProgramUnit root) throws IOException {
        MustacheCompiler mustacheCompiler = new MustacheCompiler(new File(mustacheDataFile));
        MustacheListenerImpl impl = mustacheCompiler.compile();

        MustacheWriter writer = getMustacheWriter();
        StringBuffer sb = new StringBuffer();
        writer.write(sb, new ArrayList<>(), root, impl.getTemplate(), BaseSection.SectionType.Normal);
        return CodeFormator.formatCode(sb.toString());
    }

    private static MustacheWriter getMustacheWriter() {
        MustacheWriter writer = new MustacheWriter();
        writer.getExprEvaluator().setEnvironment(new DefaultEnvironment() {
            @Override
            public void addDefault() {
                super.addDefault();
                Func func = (Func) getVar("__System_Function");
                if(func == null){
                    func = new Func();
                    setVar("__System_Function", func);
                }
                addFunction("dim_push", args -> ((Func) getVar("__System_Function")).dim_push((Number) args[0]));
                addFunction("dim_pop", args -> ((Func) getVar("__System_Function")).dim_pop());
                addFunction("dim_peek", args -> ((Func) getVar("__System_Function")).dim_peek());
                addFunction("dim_size", args -> ((Func) getVar("__System_Function")).dim_size());
                addFunction("dim_value", args -> ((Func) getVar("__System_Function")).dim_value());
                addFunction("dim_putQlfLevel", args -> ((Func) getVar("__System_Function")).dim_putQlfLevel((String) args[0], (String) args[1]));
                addFunction("dim_getQlfLevel", args -> ((Func) getVar("__System_Function")).dim_getQlfLevel((String) args[0]));
                addFunction("str_replace", args -> ((String) args[0]).replace((String) args[1], (String) args[2]));
                addFunction("name_toField", args -> ((Func) getVar("__System_Function")).name_toField((String) args[0]));
                addFunction("name_toClass", args -> ((Func) getVar("__System_Function")).name_toClass((String) args[0]));
                addFunction("name_enterClass", args -> ((Func) getVar("__System_Function")).name_enterClass((String) args[0]));
                addFunction("name_exitClass", args -> ((Func) getVar("__System_Function")).name_exitClass());
                addFunction("name_putInnerField", args -> ((Func) getVar("__System_Function")).name_putInnerField((String) args[0]));
                addFunction("name_delegateName", args -> ((Func) getVar("__System_Function")).name_delegateName((String) args[0]));
                addFunction("name_delegateName1", args -> ((Func) getVar("__System_Function")).name_delegateName1((String) args[0], (String) args[1]));
                addFunction("expr_convertExpr", args -> ((Func) getVar("__System_Function")).expr_convertExpr((String) args[0]));
                addFunction("array_initString", args -> ((Func) getVar("__System_Function")).array_initString(args[0].toString(), args[1].toString()));
                addFunction("type_getType", args -> ((Func) getVar("__System_Function")).type_getType((String) args[0]));
            }
        });
        writer.getExprEvaluator().setVar("model_package", "free.test");
        writer.setPartialFileHandler(new IPartialFileHandler() {
            @Override
            public Template compilePartialTemplate(String partialName) {
                URL url = TestRecursion.class.getResource(File.separator + partialName.replace(".",File.separator) + ".mustache");
                try {
                    MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
                    try {
                        return mustacheCompiler.compile().getTemplate();
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                } catch (Throwable e) {
                    throw new RuntimeException(e);
                }
            }
        });
        return writer;
    }
}
