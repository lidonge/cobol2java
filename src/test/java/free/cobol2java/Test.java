package free.cobol2java;

import free.servpp.multiexpr.handler.ExprEvaluator;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.IOException;

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

    private static void convert(String file, String name){
        Cobol2Java cobol2Java = new Cobol2Java(file, name,"free.test");
        String prog = cobol2Java.convertAll();
        System.out.println(prog);
    }
    private static void convert(String file, String name, CobolPreprocessor.CobolSourceFormatEnum format, String encoding){
        Cobol2Java cobol2Java = new Cobol2Java(file, name,"free.test",format,encoding);
        String prog = cobol2Java.convertAll();
        System.out.println(prog);
    }
    public static void main(String[] args) throws IOException {
        testCost("File",() -> convert(cblDir + "File.cbl", "File"));

        if (false) {
            testCost("Data",() -> convert(cblDir + "Data.cbl", "Data"));
            testCost("Accept",() -> convert(cblDir + "Accept.cbl", "Accept"));
            testCost("Init",() -> convert(cblDir + "Init.cbl", "Init"));
            testCost("Move",() -> convert(cblDir + "Move.cbl", "Move"));
            testCost("Add",() -> convert(cblDir + "Add.cbl", "Add"));
            testCost("Subtract",() -> convert(cblDir + "Subtract.cbl", "Subtract"));
            testCost("Multiply",() -> convert(cblDir + "Multiply.cbl", "Multiply"));
            testCost("Divide",() -> convert(cblDir + "Divide.cbl", "Divide"));
            testCost("Compute",() -> convert(cblDir + "Compute.cbl", "Compute"));
            testCost("Table",() -> convert(cblDir + "Table.cbl", "Table"));
            testCost("Perform",() -> convert(cblDir + "Perform.cbl", "Perform"));
            testCost("CallSub",() -> convert(cblDir + "CallSub.cbl", "CallSub"));
            testCost("SubProg",() -> convert(cblDir + "SubProg.cbl", "SubProg"));
            testCost("IfClause",() -> convert(cblDir + "IfClause.cbl", "IfClause"));
            testCost("String",() -> convert(cblDir + "String.cbl", "String"));
            testCost("Sql",() -> convert(cblDir + "Sql.cbl", "Sql"));
            testCost("File",() -> convert(cblDir + "File.cbl", "File"));
            testCost("EXAMPLE",() -> convert(cblDir + "Example.cbl", "EXAMPLE"));
            testCost("ALL",() -> convert(cblDir + "All.cbl", "All"));
            testCost("GSA01060NC",() -> convert(cblDir + "GSA01060NC.cbl", "GSA01060NC",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
            testCost("GSA01060",() -> convert(cblDir + "GSA01060.cbl", "GSA01060",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
            testCost("SAACNACN",() -> convert(cblDir + "sql/SAACNACN.sqb", "SAACNACN",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
        }
    }
}
