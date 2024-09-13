package free.cobol2java;

import free.cobol2java.copybook.CopyBookManager;
import free.servpp.mustache.CodeFormator;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static free.cobol2java.copybook.ICobol2JavaBase.COPYBOOK_CONTEXT;

/**
 * @author lidong@date 2024-07-30@version 1.0
 */
public class Test extends BaseConvertor{
    static String cblDir = "/Users/lidong/gitspace/cobol2java/convertor/src/main/COBOL/";

    public static void testCost(String name, Runnable runnable){
        long curTime = System.currentTimeMillis();
        runnable.run();
        System.out.println("Execute " +name +" Cost:" + (System.currentTimeMillis() - curTime)/1000.0);
    }

    private static void convert(String file, String name){
        convert(file,name, CobolPreprocessor.CobolSourceFormatEnum.TANDEM,"utf-8");
    }
    private static void convert(String file, String name, CobolPreprocessor.CobolSourceFormatEnum format, String encoding){
        List<File> copyDirs = new ArrayList<>();
        copyDirs.add(new File(cblDir +"/bank/copy"));
        copyDirs.add(new File(cblDir +"/demo"));
        Cobol2Java cobol2Java = new Cobol2Java(file, name,copyDirs,"free.test",format,encoding);
        Map<String,Object> varables = new HashMap<>();
        CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
        if(defaultManager.isCopybookManage()) {
            varables.put(COPYBOOK_CONTEXT,defaultManager.getGlobalFunc());
        }
        String prog = cobol2Java.convertAll(varables);
        System.out.println(prog);
        System.out.println("===================");
        for(Map.Entry entry : defaultManager.getCopyBookMap().entrySet()){
            System.out.println(CodeFormator.formatCode(entry.getValue().toString()));
        }
    }
    public static void main(String[] args) throws IOException {
        testCost("TestAccept",() -> convert(cblDir + "demo/TESTACCEPT.cbl", "TestAccept"));
        if (false) {
            testCost("TestData",() -> convert(cblDir + "demo/TESTDATA.cbl", "TestData"));
            testCost("TestAccept",() -> convert(cblDir + "demo/TESTACCEPT.cbl", "TestAccept"));
            testCost("Init",() -> convert(cblDir + "demo/INIT.cbl", "Init"));
            testCost("TestMove",() -> convert(cblDir + "demo/TESTMOVE.cbl", "TestMove"));
            testCost("TestAdd",() -> convert(cblDir + "demo/TESTADD.cbl", "TestAdd"));
            testCost("TestSubtract",() -> convert(cblDir + "demo/TESTSUBTRACT.cbl", "TestSubtract"));
            testCost("TestMultiply",() -> convert(cblDir + "demo/TESTMULTIPLY.cbl", "TestMultiply"));
            testCost("TestDivide",() -> convert(cblDir + "demo/TESTDIVIDE.cbl", "TestDivide"));
            testCost("TestCompute",() -> convert(cblDir + "demo/TESTCOMPUTE.cbl", "TestCompute"));
            testCost("TestTable",() -> convert(cblDir + "demo/TESTTABLE.cbl", "TestTable"));
            testCost("TestPerform",() -> convert(cblDir + "demo/TESTPERFORM.cbl", "TestPerform"));
            testCost("CallSub",() -> convert(cblDir + "demo/CALLSUB.cbl", "CallSub"));
            testCost("SubProg",() -> convert(cblDir + "demo/SUBPROG.cbl", "SubProg"));
            testCost("IfClause",() -> convert(cblDir + "demo/IFCLAUSE.cbl", "IfClause"));
            testCost("TestString",() -> convert(cblDir + "demo/TESTSTRING.cbl", "TestString"));
            testCost("TestSql",() -> convert(cblDir + "demo/TESTSQL.cbl", "TestSql"));
            testCost("TestFile",() -> convert(cblDir + "demo/TESTFILE.cbl", "TestFile"));
            testCost("EXAMPLE",() -> convert(cblDir + "demo/EXAMPLE.cbl", "EXAMPLE"));
            testCost("TestAll",() -> convert(cblDir + "demo/TESTALL.cbl", "TestAll"));
//            testCost("PSACONST",() -> convert(cblDir + "bank/copy/PSACONST", "PSACONST",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
            testCost("GSA01060NC",() -> convert(cblDir + "bank/GSA01060NC.cbl", "GSA01060NC",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
            testCost("GSA01060",() -> convert(cblDir + "bank/GSA01060.cbl", "GSA01060",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
            testCost("SAACNACN",() -> convert(cblDir + "bank/sql/SAACNACN.sqb", "SAACNACN",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
            testCost("GDBI4RDB",() -> convert(cblDir + "bank/sql/GDBI4RDB.cbl", "GDBI4RDB",CobolPreprocessor.CobolSourceFormatEnum.FIXED, "gb2312"));
        }
    }
}
