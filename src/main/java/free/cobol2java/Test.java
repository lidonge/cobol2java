package free.cobol2java;

import free.cobol2java.util.Func;
import free.servpp.multiexpr.handler.ExprEvaluator;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.TestRecursion;
import free.servpp.multiexpr.handler.DefaultEnvironment;
import free.servpp.mustache.handler.IPartialFileHandler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import free.servpp.mustache.model.BaseSection;
import free.servpp.mustache.model.Template;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-07-30@version 1.0
 */
public class Test {
    static String cblDir = "/Users/lidong/gitspace/cobol2java/src/main/COBOL/";
    static String mustacheDataFile = "/Users/lidong/gitspace/cobol2java/src/main/resources/model.mustache";
    static String mustacheProgramFile = "/Users/lidong/gitspace/cobol2java/src/main/resources/program.mustache";
    static ExprEvaluator exprEvaluator;

    public static void testCost(String name, Runnable runnable){
        long curTime = System.currentTimeMillis();
        runnable.run();
        System.out.println("Execute " +name +" Cost:" + (System.currentTimeMillis() - curTime)/1000.0);
    }
    public static void main(String[] args) throws IOException {
        testCost("ALL",() -> convertAll(cblDir + "All.cbl", "All"));

        if (false) {
            testCost("Data",() -> convertAll(cblDir + "Data.cbl", "Data"));
            testCost("Accept",() -> convertAll(cblDir + "Accept.cbl", "Accept"));
            testCost("Init",() -> convertAll(cblDir + "Init.cbl", "Init"));
            testCost("Move",() -> convertAll(cblDir + "Move.cbl", "Move"));
            testCost("Add",() -> convertAll(cblDir + "Add.cbl", "Add"));
            testCost("Subtract",() -> convertAll(cblDir + "Subtract.cbl", "Subtract"));
            testCost("Multiply",() -> convertAll(cblDir + "Multiply.cbl", "Multiply"));
            testCost("Divide",() -> convertAll(cblDir + "Divide.cbl", "Divide"));
            testCost("Compute",() -> convertAll(cblDir + "Compute.cbl", "Compute"));
            testCost("Table",() -> convertAll(cblDir + "Table.cbl", "Table"));
            testCost("Perform",() -> convertAll(cblDir + "Perform.cbl", "Perform"));
            testCost("CallSub",() -> convertAll(cblDir + "CallSub.cbl", "CallSub"));
            testCost("SubProg",() -> convertAll(cblDir + "SubProg.cbl", "SubProg"));
            testCost("IfClause",() -> convertAll(cblDir + "IfClause.cbl", "IfClause"));
            testCost("String",() -> convertAll(cblDir + "String.cbl", "String"));
            testCost("Sql",() -> convertAll(cblDir + "Sql.cbl", "Sql"));
            testCost("EXAMPLE",() -> convertAll(cblDir + "Example.cbl", "EXAMPLE"));
            testCost("ALL",() -> convertAll(cblDir + "All.cbl", "All"));
            testCost("GSA01060NC",() -> convertAll(cblDir + "GSA01060NC.cbl", "GSA01060NC",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
        }
    }

    private static void convertAll(String cblFile, String progName) {
        CobolPreprocessor.CobolSourceFormatEnum format = CobolPreprocessor.CobolSourceFormatEnum.TANDEM;
        convertAll(cblFile, progName, format,null);
    }

    private static void convertAll(String cblFile, String progName, CobolPreprocessor.CobolSourceFormatEnum format, String encoding)  {
        try {
            CompilationUnit compilationUnit = null;
            compilationUnit = getProgram(cblFile, progName, format, encoding);
            ProgramUnit programUnit = compilationUnit.getProgramUnit();
            String prog = convertProgram(programUnit);
            System.out.println(prog);
            compilationUnit =compilationUnit;
        } catch (IOException e) {
            e.printStackTrace();
        }
//            new ObjectTreePrinter().printObjectTree(compilationUnit);
//        new ObjectTreePrinter().printObjectTree(programUnit);
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

        MustacheWriter writer = getMustacheWriter(programUnit);
        writer.write(impl.getTemplate(), BaseSection.SectionType.Normal);
        StringBuffer sb = writer.getOutText();

        return CodeFormator.formatCode(sb.toString());
    }

    static String convertData(ProgramUnit root) throws IOException {
        MustacheCompiler mustacheCompiler = new MustacheCompiler(new File(mustacheDataFile));
        MustacheListenerImpl impl = mustacheCompiler.compile();

        MustacheWriter writer = getMustacheWriter(root);
        writer.write(impl.getTemplate(), BaseSection.SectionType.Normal);
        StringBuffer sb = writer.getOutText();
        return CodeFormator.formatCode(sb.toString());
    }

    private static MustacheWriter getMustacheWriter(Object root) {
        MustacheWriter writer = new MustacheWriter(root){
            {
                exprEvaluator = this.getExprEvaluator();
                exprEvaluator.setEnvironment(new MustacheEnvironment() {
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
                        addFunction("name_getFieldType", args -> ((Func) getVar("__System_Function")).name_getFieldType((String) args[0]));
                        addFunction("name_setFieldType", args -> ((Func) getVar("__System_Function")).name_setFieldType((String) args[0], (String) args[1]));
                        addFunction("name_getFieldClsType", args -> ((Func) getVar("__System_Function")).name_getFieldClsType((String) args[0]));
                        addFunction("name_setFieldClsType", args -> ((Func) getVar("__System_Function")).name_setFieldClsType((String) args[0], (String) args[1]));
                        addFunction("expr_convertExpr", args -> ((Func) getVar("__System_Function")).expr_convertExpr((String) args[0]));
                        addFunction("rel_getOper", args -> ((Func) getVar("__System_Function")).rel_getOper(args[0]+"", (String) args[1], (String) args[2]));
                        addFunction("array_initString", args -> ((Func) getVar("__System_Function")).array_initString(args[0].toString(), args[1].toString()));
                        addFunction("type_getType", args -> ((Func) getVar("__System_Function")).type_getType((String) args[0]));
                        addFunction("cbl_getComment", args -> ((Func) getVar("__System_Function")).cbl_getComment((Integer) args[0], (String) args[1], (List<String>) args[2]));
                    }
                });
            }

        };
        exprEvaluator.setVar("model_package", "free.test");
        writer.setPartialFileHandler(new IPartialFileHandler() {
            Map<String,Template> templateMap = new HashMap<>();
            String lastName = "";
            @Override
            public Template compilePartialTemplate(String partialName) {
                Template tmpl = templateMap.get(partialName);

                if(tmpl == null) {
                    URL url = TestRecursion.class.getResource(File.separator + partialName.replace(".", File.separator) + ".mustache");
                    try {
                        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
                        try {
//                            System.out.println("Reading mustache template :" + partialName);

                            lastName = partialName;
                            tmpl = mustacheCompiler.compile().getTemplate();
                            templateMap.put(partialName,tmpl);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    } catch (Throwable e) {
                        throw new RuntimeException(e);
                    }
                }
                return tmpl;
            }
        });
        return writer;
    }
}
