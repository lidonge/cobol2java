package free.optimize;

import com.github.javaparser.ParseProblemException;
import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.*;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * @author lidong@date 2025-02-14@version 1.0
 */
public class BaseTool {
    // Instance variables (class members)
    protected final Map<String, ClassOrInterfaceDeclaration> classCache = new HashMap<>();
    // Cache all classes in the project directory
    public void cacheAllClasses(File rootDir) {
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
                    cacheClassDefinitions(currentFile);
                }
            }
        }
    }

    // Cache class definitions for resolving types
    private void cacheClassDefinitions(File file) {
        try {
            CompilationUnit cu = StaticJavaParser.parse(file);

            cu.findAll(ClassOrInterfaceDeclaration.class).forEach(cls -> {

                String type = cls.getNameAsString();
                String innerType = getInnerClassType(cu,type);
                if(innerType != null)
                    type = innerType;
                if(classCache.get(type) != null) {
                    System.out.println("Error: Duplicate class name: " + type);
                }
                classCache.put(type, cls);
            });
        } catch (IOException | ParseProblemException e) {
            System.err.println("Error parsing file: " + file.getPath());
        }
    }
    // Resolve the type of a field (e.g., pcecacnd.oRtrnCode)
    public String resolveFieldType(Expression expr, CompilationUnit cu) {
        if (expr instanceof ArrayAccessExpr)
            expr = ((ArrayAccessExpr) expr).getName();
        if (expr instanceof NameExpr) {
            String typeName = findVariableType(((NameExpr) expr).getNameAsString(), cu);
            return resolveFullQualifiedName(cu, typeName);
        } else if (expr instanceof FieldAccessExpr) {
            FieldAccessExpr fieldAccess = (FieldAccessExpr) expr;

            // Split the expression by "." to get each level (e.g., a, b, c, d)
            String[] parts = fieldAccess.toString().split("\\.");
            if (parts[0].equals("CobolConstant")) {
                return "free.cobol2java.java.CobolConstant";
            }
            // Start resolving from the first part (e.g., a)
            return resolveFieldTypeRecursive(parts, cu);
        } else {

            if (expr instanceof MethodCallExpr) {
                return expr.toString().indexOf("Util.subvalue") != -1 ? "String" : null;
            }
        }
        return null;
    }

    private String resolveFieldTypeRecursive(String[] parts, CompilationUnit cu) {
        String objectName = parts[0];
        // Find the object type
        String objectType = findVariableType(objectName, cu);

        // If we're at the top level (currentType is null), start the search from the root
        if (parts.length == 2) {
            // If the first part is found as a variable in the CompilationUnit
            if (objectType != null) {
                // Find the field type in the object's class
                return findFieldTypeInClass(objectType, parts[1]);
            }
        } else {
            ClassOrInterfaceDeclaration cls = classCache.get(objectType);

            CompilationUnit cuNext = cls.findCompilationUnit().get();
            if (cuNext != null) {
                return resolveFieldTypeRecursive(Arrays.copyOfRange(parts, 1, parts.length), cuNext);
            }
        }

        return null;  // If no matching field was found, return null
    }

    // Find the type of a variable within the current context
    private String findVariableType(String variableName, CompilationUnit cu) {
        List<VariableDeclarator> vars = cu.findAll(VariableDeclarator.class);
        for (VariableDeclarator var : vars) {
            if (var.getNameAsString().equals(variableName)) {

                String type = var.getType().asString();
                String innerType = getInnerClassType(cu, type);
                if (innerType != null)
                    type = innerType;
                return type;
            }
        }
        return null;
    }

    protected String getInnerClassType(CompilationUnit cu, String type) {
        for (ClassOrInterfaceDeclaration cls : cu.findAll(ClassOrInterfaceDeclaration.class)) {
            if (cls.getNameAsString().equals(type)) {
                Node node = cls.getParentNode().get();
                if (node instanceof ClassOrInterfaceDeclaration) {
                    ClassOrInterfaceDeclaration classOrInterfaceDeclaration = (ClassOrInterfaceDeclaration) node;
                    return classOrInterfaceDeclaration.getNameAsString() + "." + type;
                }
            }
        }
        return null;
    }

    // Find the type of a field in a class
    private String findFieldTypeInClass(String className, String fieldName) {
        // Retrieve the class definition from the cache
        ClassOrInterfaceDeclaration cls = classCache.get(className);
        if (cls != null) {
            // Iterate through the fields of the class
            for (FieldDeclaration field : cls.getFields()) {
                for (VariableDeclarator var : field.getVariables()) {
                    if (var.getNameAsString().equals(fieldName)) {
                        // Get the field's type
                        String fieldType = var.getType().toString();
                        if (fieldType != null && fieldType.endsWith("[]"))
                            fieldType = fieldType.substring(0, fieldType.length() - 2);
                        String prmType = inferType(fieldType);

                        if (prmType == null) {
                            // Try to resolve the full qualified name of the type
                            Optional<CompilationUnit> cu = cls.findCompilationUnit();
                            if (cu.isPresent()) {
                                String fullQualifiedName = resolveFullQualifiedName(cu.get(), fieldType);
                                if (fullQualifiedName != null) {
                                    return fullQualifiedName;
                                }
                            }
                        } else {
                            fieldType = prmType;
                        }

                        // If the full qualified name cannot be resolved, return the type as is
                        return fieldType;
                    }
                }
            }
        }
        return null; // Field or class not found
    }

    private String inferType(String fieldType) {
        String ret = null;
        switch (fieldType) {
            case "String":
            case "Integer":
            case "Double":
            case "Object":
                ret = fieldType;
                break;
        }
        return ret;
    }

    // Helper function to resolve the full qualified name of a type
    private String resolveFullQualifiedName(CompilationUnit cu, String typeName) {
        if (inferType(typeName) != null) {
            return typeName;
        }

        // Check imports in the compilation unit
        for (ImportDeclaration importDecl : cu.getImports()) {
            if (importDecl.getNameAsString().endsWith("." + typeName)) {
                return importDecl.getNameAsString();
            }
        }
        //check nested class
        Optional<String> nestedClassName = findNestedClassFullName(cu, typeName);
        if (nestedClassName.isPresent()) {
            return nestedClassName.get();
        }
        // Check if the type is in the same package as the class
        if (cu.getPackageDeclaration().isPresent()) {
            return cu.getPackageDeclaration().get().getNameAsString() + "." + typeName;
        }

        return null;
    }

    // check nested class
    private Optional<String> findNestedClassFullName(CompilationUnit cu, String typeName) {
        for (ClassOrInterfaceDeclaration classDecl : cu.findAll(ClassOrInterfaceDeclaration.class)) {
            if (classDecl.getNameAsString().equals(typeName)) {
                Optional<Node> parentNode = classDecl.getParentNode();
                StringBuilder fullName = new StringBuilder(classDecl.getNameAsString());
                while (parentNode.isPresent()) {
                    if (parentNode.get() instanceof ClassOrInterfaceDeclaration) {
                        ClassOrInterfaceDeclaration parentClass = (ClassOrInterfaceDeclaration) parentNode.get();
                        fullName.insert(0, parentClass.getNameAsString() + ".");
                    } else if (parentNode.get() instanceof CompilationUnit) {
                        CompilationUnit parentCu = (CompilationUnit) parentNode.get();
                        if (parentCu.getPackageDeclaration().isPresent()) {
                            fullName.insert(0, parentCu.getPackageDeclaration().get().getNameAsString() + ".");
                        }
                    }
                    parentNode = parentNode.get().getParentNode();
                }
                return Optional.of(fullName.toString());
            }
        }
        return Optional.empty();
    }
}
