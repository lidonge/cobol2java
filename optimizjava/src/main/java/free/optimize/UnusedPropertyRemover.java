package free.optimize;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.stmt.ExpressionStmt;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * A utility class to remove unused properties (fields) from Java class files.
 * It analyzes the fields of a class, determines if they are used, and removes unused fields and related statements.
 *
 * @author lidong
 * @date 2024-12-06
 * @version 1.0
 */
public class UnusedPropertyRemover {

    public static void main(String[] args) throws IOException {
        // To process a single file for testing, uncomment the following line
        // handOne();
        // return;

        // Directory to scan for Java files
        String dir = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java";

        // Filter for Java files that meet certain criteria
        JavaFileFilter fileFilter = new JavaFileFilter("public void procedure(");
        List<File> files = fileFilter.find(dir);

        // Process each file in the directory
        for (File file : files) {
            handleAFile(file);
        }
    }

    /**
     * Handles a single file (test method for debugging).
     *
     * @throws IOException If the file cannot be read or written.
     */
    private static void handOne() throws IOException {
        String path = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/ccbmain/cbl/Gsystime1.java";
        handleAFile(new File(path));
    }

    /**
     * Parses and processes a single Java file to remove unused properties.
     *
     * @param file The Java file to process.
     * @throws IOException If the file cannot be read or written.
     */
    private static void handleAFile(File file) throws IOException {
        // Parse the file into a CompilationUnit
        CompilationUnit compilationUnit = StaticJavaParser.parse(file);

        // Process the main class and its nested classes
        processClassOrInnerClass(compilationUnit);

        // Optimize the imports after modifying the file
        new ImportsOptimizer().optimize(compilationUnit);

        // Write the modified CompilationUnit back to the file
        try (FileWriter writer = new FileWriter(file)) {
            writer.write(compilationUnit.toString());
        }
    }

    /**
     * Processes a class or an inner class to identify and remove unused fields and related statements.
     *
     * @param node The class, inner class, or compilation unit to process.
     */
    private static void processClassOrInnerClass(Node node) {
        if (node instanceof ClassOrInterfaceDeclaration) {
            ClassOrInterfaceDeclaration clazz = (ClassOrInterfaceDeclaration) node;

            // Collect all declared property (field) names
            Set<String> fieldNames = new HashSet<>();
            List<FieldDeclaration> fields = clazz.findAll(FieldDeclaration.class);
            for (FieldDeclaration field : fields) {
                for (VariableDeclarator var : field.getVariables()) {
                    fieldNames.add(var.getNameAsString());
                }
            }

            // Collect names of properties that are actually referenced in the class
            Set<String> referencedFields = new HashSet<>();
            clazz.accept(new ReferenceVisitor(fieldNames, referencedFields, fields), null);

            // Determine which fields are unused
            Set<String> unusedFields = new HashSet<>(fieldNames);
            unusedFields.removeAll(referencedFields);

            // Remove unused fields
            Iterator<FieldDeclaration> fieldIterator = fields.iterator();
            while (fieldIterator.hasNext()) {
                FieldDeclaration field = fieldIterator.next();
                boolean toRemove = false;
                for (VariableDeclarator var : field.getVariables()) {
                    if (unusedFields.contains(var.getNameAsString())) {
                        toRemove = true;
                        break;
                    }
                }
                if (toRemove) {
                    field.remove(); // Remove the field from the syntax tree
                }
            }

            // Remove assignment statements related to unused fields
            List<ExpressionStmt> expressions = clazz.findAll(ExpressionStmt.class);
            Iterator<ExpressionStmt> expressionIterator = expressions.iterator();
            while (expressionIterator.hasNext()) {
                ExpressionStmt expressionStmt = expressionIterator.next();
                if (expressionStmt.getExpression().isAssignExpr()) {
                    String target = expressionStmt.getExpression().asAssignExpr().getTarget().toString();
                    String[] parts = target.split("\\.");
                    for (String part : parts) {
                        if (unusedFields.contains(part)) {
                            expressionStmt.remove(); // Remove the assignment statement
                            break;
                        }
                    }
                }
            }

            // Recursively process nested classes
            for (BodyDeclaration<?> member : clazz.getMembers()) {
                if (member instanceof ClassOrInterfaceDeclaration &&
                        ((ClassOrInterfaceDeclaration) member).getName().getId().endsWith("Handler")) {
                    processClassOrInnerClass(member);
                }
            }
        } else if (node instanceof CompilationUnit) {
            // Process top-level classes
            for (TypeDeclaration<?> type : ((CompilationUnit) node).getTypes()) {
                if (type instanceof ClassOrInterfaceDeclaration) {
                    processClassOrInnerClass(type);
                }
            }
        }
    }
}