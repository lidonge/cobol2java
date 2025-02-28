package free.cobol2java;

import free.cobol2java.config.Cobol2javaConfig;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.context.IExprCallContext;
import free.cobol2java.copybook.CopyBookManager;
import free.cobol2java.util.CobolFilePreprocess;
import free.servpp.multiexpr.ReflectTool;
import free.servpp.mustache.CodeFormator;
import io.proleap.cobol.asg.metamodel.impl.ProgramUnitElementImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-09-04@version 1.0
 */
public class BatchConvertor extends BaseConvertor {

    public static void main(String[] args) throws IOException {
        long startTime = System.currentTimeMillis();
//        ReflectTool.DEBUG=true;
        ProgramUnitElementImpl.IGNORE_UNDEFINED_ERROR = true;
        BatchConvertor batchConvertor = new BatchConvertor();
        CobolConfig.setCobolConvertor(batchConvertor);
        if(args.length == 0){
            Cobol2javaConfig conf = new Cobol2javaConfig();
            batchConvertor.initConfig(conf.getManager());
        }
        else if (args.length != 2 && !"-c".equals(args[0])) {
            System.out.println("Usage: java BatchConvertor");
            System.out.println("Usage: java BatchConvertor -c <config_file>");
            return;
        }else {
            Cobol2javaConfig conf = new Cobol2javaConfig(new File(args[1]));
            batchConvertor.initConfig(conf.getManager());
        }

        batchConvertor.initCopybookManager();

        batchConvertor.convertAll();
//        batchConvertor.getLogger(BaseConvertor.class).info("Compile All cost:{}s", (System.currentTimeMillis()-startTime)/1000 );
        System.out.println("Compile All cost:"+ (System.currentTimeMillis()-startTime)/1000 +" seconds.");
    }

    private void initCopybookManager() {
        CopyBookManager.initDefaultManager(this);
    }


    private void convertAll() {
        List<File> files = new ArrayList<>();
        findFiles(new File(getSourcePath()), files, getSuffixes());

        for (File file : files) {
            String relativePath = file.getAbsolutePath().substring(getSourcePath().length() + 1);
            if (!isMatchCompileFiles(relativePath))
                continue;
            convertAFile(file);
            IExprCallContext.compileAllSub();
        }
    }
}
