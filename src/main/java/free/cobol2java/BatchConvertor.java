package free.cobol2java;

import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong@date 2024-09-04@version 1.0
 */
public class BatchConvertor {

    public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println("Usage: java BatchConvertor <sourcePath> <targetPath> <rootPackageName> <format> <encoding>?");
            return;
        }
//        "/Users/lidong/gitspace/cobol2java/src/main/COBOL"
//        "/Users/lidong/gitspace/cobol2java/target/generated-sources"
//        "com.dcits"
        String sourcePath = args[0];
        String targetPath = args[1];
        String rootPackageName = args[2];
        String format = args[3];
        String encoding = args.length == 5 ? args[4] : "utf-8";

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
                String convertedContent = convert(file, packageName,format,encoding);

                // Write the result to the target file
                writeToFile(outputFilePath, convertedContent);
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    // Write the converted content to the target file
    private static void writeToFile(String outputFilePath, String content) {
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
    private static String convert(File sourceFile, String packageName, String format, String encoding) {
        // Implement your conversion logic here
        System.out.println("Converting file: " + sourceFile.getAbsolutePath());
        System.out.println("Package name: " + packageName);
        String fileName = sourceFile.getName();
        String prog = null;

        Cobol2Java cobol2Java = new Cobol2Java(sourceFile.getAbsolutePath(),
                fileName.substring(0, fileName.lastIndexOf(".")), packageName,
                CobolPreprocessor.CobolSourceFormatEnum.valueOf(format), encoding);
        prog = cobol2Java.convertAll();

        return prog;
    }

}
