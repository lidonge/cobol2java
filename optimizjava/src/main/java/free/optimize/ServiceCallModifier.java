package free.optimize;

/**
 * @author lidong@date 2025-01-23@version 1.0
 */

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ServiceCallModifier {

    public static void main(String[] args) {
        // Path to the Java file that needs to be analyzed and modified
//        String filePath = "/Users/lidong/test/cbodjava/src/main/java/cbod/java/onbb/cbl/Gsa01060.java"; // Replace with your file path
        // Specify the root directory to start traversal
        File rootDirectory = new File("/Users/lidong/test/cbodjava/src/main/java/cbod/java"); // Replace with your directory path

        // List to store all found .java files
        List<File> javaFiles = new ArrayList<>();

        // Recursively find Java files
        findJavaFiles(rootDirectory, javaFiles);

        // Print all found Java files
        List<File> filesDone = new ArrayList<>();
        for (File javaFile : javaFiles) {
            // Parse the source code file into a CompilationUnit (AST representation)
            CompilationUnit compilationUnit = null;
//            if(filesDone.contains(javaFile) || javaFile.getName().indexOf("Gsa01060") == -1)
//                continue;
            try {
                compilationUnit = StaticJavaParser.parse(javaFile);

                // Analyze and modify the target service calls
                compilationUnit.accept(new ServiceCallVisitor(filesDone), null);

                // Print the modified source code
                Files.write(javaFile.toPath(), compilationUnit.toString().getBytes());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Recursively finds .java files in the directory, ignoring the "models" directory and its subdirectories.
     *
     * @param directory The current directory to search.
     * @param javaFiles The list to store found .java files.
     */
    private static void findJavaFiles(File directory, List<File> javaFiles) {
        if (directory == null || !directory.exists()) {
            return;
        }

        // List all files and subdirectories in the current directory
        File[] files = directory.listFiles();
        if (files == null) {
            return;
        }

        for (File file : files) {
            if (file.isDirectory()) {
                // Skip the "models" directory and its subdirectories
                if ("models".equalsIgnoreCase(file.getName())) {
                    continue;
                }
                // Recursively search in subdirectories
                findJavaFiles(file, javaFiles);
            } else if (file.isFile() && file.getName().endsWith(".java")) {
                // Add .java files to the list
                javaFiles.add(file);
            }
        }
    }
}