package free.optimize;

/**
 * @author lidong@date 2025-01-16@version 1.0
 */

import com.github.javaparser.ParseProblemException;
import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.visitor.VoidVisitorAdapter;
import free.servpp.mustache.CodeFormator;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class CopyCastExtractor extends BaseTool {
    private String projectPath;
    private Path outputPath;
    private Map<String, String> methodDefinitions;

    // Constructor to initialize the class
    public CopyCastExtractor(String projectPath, Path outputPath) {
        this.projectPath = projectPath;
        this.outputPath = outputPath;
        this.methodDefinitions = new HashMap<>();
    }

    // Main processing method
    public void process() throws IOException {
        File rootDir = new File(projectPath);

        // Step 1: Cache all class definitions
        cacheAllClasses(rootDir);

        // Step 2: Process each file to find Util.copyCast calls
        findAllTargetMethods(rootDir);

        // Step 3: Generate the CopyCast class file
        generateCopyCastClass();
        System.out.println("CopyCast.java file has been generated!");
    }


    // Process each Java file to find Util.copyCast calls
    private void findAllTargetMethods(File rootDir) {
        if (rootDir.exists() && rootDir.isDirectory()) {
            Queue<File> filesToProcess = new LinkedList<>();
            filesToProcess.add(rootDir);

            while (!filesToProcess.isEmpty()) {
                File currentFile = filesToProcess.poll();

                if (currentFile.isDirectory()) {
                    File[] subFiles = currentFile.listFiles();
                    if (subFiles != null) {
                        for (File subFile : subFiles) {
                            filesToProcess.add(subFile);
                        }
                    }
                } else if (currentFile.isFile() && currentFile.getName().endsWith(".java")) {
                    processJavaFile(currentFile);
                }
            }
        }
    }

    // Process a single Java file
    private void processJavaFile(File file) {
        try {
            CompilationUnit cu = StaticJavaParser.parse(file);

            cu.accept(new VoidVisitorAdapter<Void>() {
                @Override
                public void visit(MethodCallExpr methodCall, Void arg) {
                    super.visit(methodCall, arg);

                    if (methodCall.getNameAsString().equals("copyCast") &&
                            methodCall.getScope().isPresent() &&
                            methodCall.getScope().get().toString().equals("Util.copyCaster()")) {

                        List<Expression> arguments = methodCall.getArguments();
                        if (arguments.size() == 2) {
                            Expression srcExpr = arguments.get(0);
                            String srcType = inferType(srcExpr, cu);
                            if (srcType == null)
                                srcType = resolveFieldType(srcExpr, cu);

                            Expression targetExpr = arguments.get(1);
                            String targetType = inferType(targetExpr, cu);
                            if (targetType == null)
                                targetType = resolveFieldType(targetExpr, cu);

                            if (srcType != null && targetType != null) {
                                String key = srcType + "," + targetType;
                                if (methodDefinitions.get(key) == null) {
                                    if(targetType.equals("Object") && srcType.equals("Object")){

                                    }else {
                                        String method = createCastMethod(targetType, srcType);
                                        methodDefinitions.put(key, method);
                                    }
                                }
                            }
                        }
                    }
                }
            }, null);
        } catch (IOException | ParseProblemException e) {
            System.err.println("Error parsing file: " + file.getPath());
        }
    }

    // Generate the CopyCast class file
    private void generateCopyCastClass() throws IOException {
        StringBuilder classContent = new StringBuilder();
        classContent.append("package cbod.java;\n\n");
        classContent.append("public interface ICopyCast extends free.cobol2java.java.ICopyCaster{\n\n");

        for (String method : methodDefinitions.values()) {
            classContent.append("    ").append(method).append("\n\n");
        }

        classContent.append("}\n");

        Files.write(outputPath, CodeFormator.formatCode(classContent.toString()).getBytes());
    }

    // Helper function to create a specific cast method
    private String createCastMethod(String targetType, String srcType) {
        return String.format(
                "default %s copyCast(%s src, %s target) {\n    " +
                        "return null;\n}",
                targetType, srcType, targetType
        );
    }

    // Infer the type of an expression (basic types only)
    private String inferType(Expression expr, CompilationUnit cu) {
        if (expr instanceof StringLiteralExpr) {
            return "String";
        } else if (expr instanceof IntegerLiteralExpr) {
            return "int";
        } else if (expr instanceof BooleanLiteralExpr) {
            return "boolean";
        }
        return null; // Extend this for more types
    }

    public static void main(String[] args) {
        String projectPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java"; // Change this to your project directory

        Path outputPath = Paths.get("/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/ICopyCast.java"); // Output file path
        CopyCastExtractor copyCastExtractor = new CopyCastExtractor(projectPath, outputPath);
        try {
            copyCastExtractor.process();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        ;
    }
}