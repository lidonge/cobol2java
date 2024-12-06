package free.optimize;

/**
 * @author lidong@date 2024-12-06@version 1.0
 */

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

public class UnusedPropertyRemover {
    public static void main(String[] args) throws IOException {
        if(false){
            handOne();
            return;
        }
        // Read Java files
        String dir = "/Users/lidong/test/cbodjava/src/main/java/cbod/java";
        JavaFileFilter fileFilter = new JavaFileFilter("public void procedure(");
        List<File> files = fileFilter.find(dir);

        for (File file : files) {
            handleAFile(file);
        }
    }

    private static void handOne() throws IOException {
        String path = "/Users/lidong/test/cbodjava/src/main/java/cbod/java/ccbmain/cbl/Gsystime1.java";
        handleAFile(new File(path));
    }

    private static void handleAFile(File file) throws IOException {
        CompilationUnit compilationUnit = StaticJavaParser.parse(file);

        // Process the main class and its nested classes
        processClassOrInnerClass(compilationUnit);
        new ImportsOptimizer().optimize(compilationUnit);

        // Optional: Write back the modified file
        try (FileWriter writer = new FileWriter(file)) {
            writer.write(compilationUnit.toString());
        }
    }

    private static void processClassOrInnerClass(Node node) {
        if (node instanceof ClassOrInterfaceDeclaration) {
            ClassOrInterfaceDeclaration clazz = (ClassOrInterfaceDeclaration) node;

            // Collect all property names
            Set<String> fieldNames = new HashSet<>();
            List<FieldDeclaration> fields = clazz.findAll(FieldDeclaration.class);
            for (FieldDeclaration field : fields) {
                for (VariableDeclarator var : field.getVariables()) {
                    fieldNames.add(var.getNameAsString());
                }
            }

            // Collect referenced property names
            Set<String> referencedFields = new HashSet<>();
            clazz.accept(new ReferenceVisitor(fieldNames, referencedFields, fields), null);

            // Find properties that are not actually used
            Set<String> unusedFields = new HashSet<>(fieldNames);
            unusedFields.removeAll(referencedFields);

            // Remove unused properties
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

            // Remove related assignment statements
            List<ExpressionStmt> expressions = clazz.findAll(ExpressionStmt.class);
            Iterator<ExpressionStmt> expressionIterator = expressions.iterator();
            while (expressionIterator.hasNext()) {
                ExpressionStmt expressionStmt = expressionIterator.next();
                if (expressionStmt.getExpression().isAssignExpr()) {
                    String target = expressionStmt.getExpression().asAssignExpr().getTarget().toString();
                    String[] parts = target.split("\\.");
                    for (String part : parts) {
                        if (unusedFields.contains(part)) {
                            expressionStmt.remove(); // Remove the assignment statement from the syntax tree
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