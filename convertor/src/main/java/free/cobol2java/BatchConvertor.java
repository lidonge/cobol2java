package free.cobol2java;

import free.cobol2java.config.Cobol2javaConfig;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.copybook.CopyBookManager;
import free.servpp.multiexpr.ReflectTool;
import free.servpp.mustache.CodeFormator;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static free.cobol2java.copybook.ICobol2JavaBase.COPYBOOK_CONTEXT;

/**
 * @author lidong@date 2024-09-04@version 1.0
 */
public class BatchConvertor extends BaseConvertor {

    public static void main(String[] args) throws IOException {
//        ReflectTool.DEBUG=true;
        BatchConvertor batchConvertor = new BatchConvertor();
        CobolConfig.setCobolConvertor(batchConvertor);
        if(args.length == 0){
            Cobol2javaConfig conf = new Cobol2javaConfig();
            batchConvertor.initConfig(conf.getManager());
        }
        else if (args.length < 4) {
            System.out.println("Usage: java BatchConvertor");
            System.out.println("Usage: java BatchConvertor <sourcePath> <targetPath> <copyDir:...CopyDir> <rootPackageName> <format> <encoding>?");
            return;
        }else
            batchConvertor.initConfig(args);

        batchConvertor.initCopybookManager();

        batchConvertor.convertAll();
        batchConvertor.writeCopyBook();
    }

    private void writeCopyBook() {
        CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
        for(Map.Entry entry : defaultManager.getCopyBookMap().entrySet()){
            String outputFilePath = getTargetPath() + File.separator +
                    getRootPackageName().replace(".", File.separator) + File.separator +
                    entry.getKey() + ".java";
            writeToFile(outputFilePath, CodeFormator.formatCode(entry.getValue()+""));
//            System.out.println(CodeFormator.formatCode(entry.getValue().toString()));
        }
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
        }
    }
}
