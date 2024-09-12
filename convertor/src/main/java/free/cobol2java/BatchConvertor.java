package free.cobol2java;

import com.typesafe.config.Config;
import free.cobol2java.config.Cobol2javaConfig;
import free.cobol2java.copybook.CopyBookManager;
import free.servpp.config.IConfig;
import free.servpp.config.hocon.HoconConfigTypeManager;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static free.cobol2java.copybook.ICobol2JavaBase.GLOBAL_FUNCTION;

/**
 * @author lidong@date 2024-09-04@version 1.0
 */
public class BatchConvertor extends BaseConvertor {

    public static void main(String[] args) throws IOException {
        BatchConvertor batchConvertor = new BatchConvertor();
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
//        "/Users/lidong/gitspace/cobol2java/src/main/COBOL"
//        "/Users/lidong/gitspace/cobol2java/target/generated-sources"
//        "com.dcits"

        batchConvertor.initCopybookManager();

        batchConvertor.convertAll();
        batchConvertor.writeCopyBook();
    }

    private void writeCopyBook() {
        CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
        for(Map.Entry entry : defaultManager.getCopyBookMap().entrySet()){
            String outputFilePath = targetPath + File.separator +
                    rootPackageName.replace(".", File.separator) + File.separator +
                    entry.getKey() + ".java";
            writeToFile(outputFilePath,CodeFormator.formatCode(entry.getValue()+""));
//            System.out.println(CodeFormator.formatCode(entry.getValue().toString()));
        }
    }

    private void initCopybookManager() {
        CopyBookManager.initDefaultManager(copyDirs,rootPackageName,format,encoding,copybookManage);
    }


    private void convertAll() {
        List<File> files = new ArrayList<>();
        findFiles(new File(sourcePath), files);

        for (File file : files) {
            String relativePath = file.getAbsolutePath().substring(sourcePath.length() + 1);
            if (!relativePath.endsWith(".cbl"))
                continue;
            String outputFilePath = targetPath + File.separator +
                    rootPackageName.replace(".", File.separator) + File.separator +
                    relativePath.substring(0, relativePath.lastIndexOf(".")) + ".java";
            String relativeParent = new File(relativePath).getParent();
            String packageName = rootPackageName +
                    (relativeParent != null ? "." + relativeParent.replace(File.separator, ".") : "");

            // Call the convert function and get the result as a string
            try {
                String convertedContent = convert(file, packageName);

                // Write the result to the target file
                writeToFile(outputFilePath, convertedContent);
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    // Write the converted content to the target file
    private void writeToFile(String outputFilePath, String content) {
        File targetFile = new File(outputFilePath);
        targetFile.getParentFile().mkdirs();  // Create directories if they don't exist

        try (FileWriter writer = new FileWriter(targetFile)) {
            writer.write(content);
            writer.close();
            System.out.println("Written to file: " + targetFile.getAbsolutePath());
        } catch (IOException e) {
            System.err.println("Error writing to file: " + targetFile.getAbsolutePath());
            e.printStackTrace();
        }
    }

    // Recursively finds all files in the directory and subdirectories
    private static void findFiles(File directory, List<File> files) {
        if (directory.isDirectory()) {
            for (File file : directory.listFiles()) {
                if (file.isDirectory()) {
                    findFiles(file, files);
                } else {
                    files.add(file);
                }
            }
        }
    }

    // Dummy convert function (you can replace it with your actual logic)
    private String convert(File sourceFile, String packageName) {
        // Implement your conversion logic here
        System.out.println("Converting file: " + sourceFile.getAbsolutePath());
        System.out.println("Package name: " + packageName);
        String fileName = sourceFile.getName();
        String prog = null;

        Cobol2Java cobol2Java = new Cobol2Java(sourceFile.getAbsolutePath(),
                fileName.substring(0, fileName.lastIndexOf(".")),copyDirs, packageName,
                CobolPreprocessor.CobolSourceFormatEnum.valueOf(format), encoding);
        CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
        Map<String,Object> varables = new HashMap<>();
        if(defaultManager.isCopybookManage()) {
            varables.put(GLOBAL_FUNCTION,defaultManager.getGlobalFunc());
        }
        prog = cobol2Java.convertAll(varables);

        return prog;
    }

}
