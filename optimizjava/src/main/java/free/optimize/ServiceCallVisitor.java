package free.optimize;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.visitor.VoidVisitorAdapter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2025-01-24@version 1.0
 */

/**
 * Custom visitor class to locate and modify service calls
 */

class ServiceCallVisitor extends VoidVisitorAdapter<Void> {
    // Base directory containing the Java source files
    private static final String SOURCE_DIRECTORY = "/Users/lidong/test/cbodjava/src/main/java";
    private static final String CBAPALST_PROP = "apaArea";
    public static final String CBAPALST = "Cbapalst";
    public static final String CBAPALST_PACK = "cbod.java.models.ccbmain.copy";
    private List<File> filesDone;
//    private Map<File,CompilationUnit> filesDone;

    public ServiceCallVisitor(List<File> filesDone) {
        this.filesDone = filesDone;
    }

    @Override
    public void visit(MethodCallExpr methodCall, Void arg) {
        super.visit(methodCall, arg);

        // Check if the method being called is "procedure"
        if (methodCall.getNameAsString().equals("procedure")) {
            // Verify if this is a call on ServiceManager.getService(...)
            if (methodCall.getScope().isPresent() && isServiceManagerCall(methodCall.getScope().get())) {
                // Get the arguments of the method call
                NodeList<Expression> arguments = methodCall.getArguments();

                // Check if any argument contains a field of type Cbapalst
                if (argumentsContainCbapalst(arguments, methodCall)) {
                    // Add a new Cbapalst parameter
                    methodCall.getArguments().add(0, StaticJavaParser.parseExpression(CBAPALST_PROP));
                    String callCls = ((ClassExpr) ((MethodCallExpr) methodCall.getScope().get()).getArgument(0)).getType().asString();
                    String paramCls = CBAPALST;
                    String paramClsPack = CBAPALST_PACK;
                    File classFile = locateClassFile(callCls);
                    if(!filesDone.contains(classFile)) {
                        // Parse the Java file
                        CompilationUnit compilationUnit = null;
                        try {
                            compilationUnit = StaticJavaParser.parse(classFile);
                            filesDone.add(classFile);
                        } catch (FileNotFoundException e) {
                            throw new RuntimeException(e);
                        }

                        // Modify the `procedure` method
                        compilationUnit.findAll(MethodDeclaration.class).forEach(method -> {
                            if (method.getNameAsString().equals("procedure")) {
                                // Add the parameter to the method
                                addParameterToMethod(method, paramCls, paramClsPack, CBAPALST_PROP);
                                // Modify constructor calls inside the method
                                method.accept(new ConstructorCallModifier(paramCls, CBAPALST_PROP), null);
                            }
                        });
                        // Modify the constructor of the inner class
                        modifyInnerClassConstructor(compilationUnit, paramCls, CBAPALST_PROP);
//                        compilationUnit.accept(new ServiceCallVisitor(filesDone), null);
                        // Write the modified code back to the file (or output it)
                        try {
                            Files.write(classFile.toPath(), compilationUnit.toString().getBytes());
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
            }
        }
    }

    /**
     * Adds a parameter to the specified method declaration.
     *
     * @param method       The method declaration to modify.
     * @param paramCls     The name of the parameter class.
     * @param paramClsPack The package of the parameter class.
     * @param variableName The variable name for the new parameter.
     */
    private void addParameterToMethod(MethodDeclaration method, String paramCls, String paramClsPack, String variableName) {
        // Fully qualified parameter class
        String fullyQualifiedParam = paramClsPack + "." + paramCls;

        // Import the class if not already imported
        CompilationUnit compilationUnit = method.findCompilationUnit().orElseThrow();
        if (compilationUnit.getImports().stream().noneMatch(importDecl -> importDecl.getNameAsString().equals(fullyQualifiedParam))) {
            compilationUnit.addImport(fullyQualifiedParam);
        }

        // Add the parameter to the method
        ClassOrInterfaceType parameterType = new ClassOrInterfaceType(null, paramCls);
        method.getParameters().add(0, new Parameter(parameterType, variableName));

        System.out.println("Added parameter: " + paramCls + " " + variableName + " to method: " + method.getNameAsString());
    }

    /**
     * Visitor to modify constructor calls inside a method.
     */
    private static class ConstructorCallModifier extends VoidVisitorAdapter<Void> {
        private final String paramCls;
        private final String variableName;

        public ConstructorCallModifier(String paramCls, String variableName) {
            this.paramCls = paramCls;
            this.variableName = variableName;
        }

        @Override
        public void visit(ObjectCreationExpr constructorCall, Void arg) {
            super.visit(constructorCall, arg);

            // Check if the constructor is for a class matching "XXXHandler"
            if (constructorCall.getType().getNameAsString().endsWith("Handler")) {
                // Add the new parameter to the constructor call
                constructorCall.getArguments().add(0, StaticJavaParser.parseExpression(variableName));

                System.out.println("Modified constructor call: " + constructorCall);
            }
        }
    }

    /**
     * Modify the constructor of the inner class.
     *
     * @param compilationUnit The CompilationUnit of the Java file.
     * @param paramCls        The name of the parameter class.
     * @param variableName    The variable name for the new parameter.
     */
    private void modifyInnerClassConstructor(CompilationUnit compilationUnit, String paramCls, String variableName) {
        // Find the inner class declaration
        compilationUnit.findAll(ClassOrInterfaceDeclaration.class).forEach(innerClass -> {
            if (innerClass.getNameAsString().endsWith("Handler")) {
                // Add the property to the inner class if it does not already exist
                String propName = addPropertyToClass(innerClass, CBAPALST, CBAPALST_PACK);
                // Find and modify all constructors of the inner class
                innerClass.findAll(ConstructorDeclaration.class).forEach(constructor -> {
                    ClassOrInterfaceType parameterType = new ClassOrInterfaceType(null, paramCls);
                    constructor.getParameters().add(0, new Parameter(parameterType, variableName));
                    // Add initialization logic for the property
                    if (!constructor.getBody().isEmpty()) {
                        constructor.getBody().addStatement("this."+propName+" = " + variableName + ";");
                    }
                    System.out.println("Modified constructor: " + constructor.getDeclarationAsString());
                });
            }
        });
    }

    /**
     * Adds a property of the given type to the specified class if it does not already exist.
     */
    private String addPropertyToClass(ClassOrInterfaceDeclaration clazz, String propertyCls, String propertyClsPack) {
        // Fully qualified property class
        String fullyQualifiedProperty = propertyClsPack + "." + propertyCls;

        // Import the class if not already imported
        CompilationUnit compilationUnit = clazz.findCompilationUnit().orElseThrow();
        if (compilationUnit.getImports().stream().noneMatch(importDecl -> importDecl.getNameAsString().equals(fullyQualifiedProperty))) {
            compilationUnit.addImport(fullyQualifiedProperty);
        }

        // Check if the property already exists
        boolean propertyExists = false;
        String ret = CBAPALST_PROP;

        // Iterate over all fields in the class
        for (FieldDeclaration field : clazz.getFields()) {
            // Iterate over all variables in the field
            for (VariableDeclarator variable : field.getVariables()) {
                // Check if the variable type matches the desired property class
                if (variable.getType().asString().equals(propertyCls)) {
                    propertyExists = true;
                    ret = variable.getNameAsString();
                    break; // Exit the inner loop as we found a match
                }
            }
            if (propertyExists) {
                break; // Exit the outer loop as we already found the property
            }
        }

        // Add the property if it does not exist
        if (!propertyExists) {
            // Create the new field
            FieldDeclaration field = new FieldDeclaration()
                    .addVariable(new VariableDeclarator(new ClassOrInterfaceType(null, propertyCls), ret))
                    .setPrivate(true);

            // Add the field at the first position
            NodeList<BodyDeclaration<?>> members = clazz.getMembers();
            members.add(0, field); // Add the field at index 0 (first position)

            System.out.println("Added property at the first position: private " + propertyCls + " " + ret+";");
        }else{
            // Remove assignments to the specified property
            String finalRet = ret;
            // Find all method declarations in the compilation unit
            List<MethodDeclaration> methods = compilationUnit.findAll(MethodDeclaration.class);
            for (MethodDeclaration method : methods) {
                // Use the visitor to remove assignments to the specified property
                PropertyAssignmentRemover remover = new PropertyAssignmentRemover(finalRet);
                method.accept(remover, null);
                if(remover.needRemove.size() != 0){
                    for (ExpressionStmt stmt:remover.needRemove){
                        stmt.remove();
                    }
                }
            }
        }
        return ret;
    }
    /**
     * Visitor to remove assignments to the specified property.
     */
    private static class PropertyAssignmentRemover extends VoidVisitorAdapter<Void> {
        private final String propertyName;
        List<ExpressionStmt> needRemove = new ArrayList<>();
        public PropertyAssignmentRemover(String propertyName) {
            this.propertyName = propertyName;
        }

        @Override
        public void visit(ExpressionStmt stmt, Void arg) {
            super.visit(stmt, arg);

            // Check if the statement is an assignment expression
            if (stmt.getExpression() instanceof AssignExpr) {
                AssignExpr assignExpr = (AssignExpr) stmt.getExpression();

                // Check if the left-hand side (target) matches the property name
                if (isTargetProperty(assignExpr.getTarget())) {
//                    stmt.remove(); // Remove the statement from the method body
                    needRemove.add(stmt);
                    System.out.println("Removed assignment: " + stmt);
                }
            }
        }

        /**
         * Check if the assignment target matches the specified property name.
         *
         * @param target The left-hand side of the assignment.
         * @return True if the target matches the property name, false otherwise.
         */
        private boolean isTargetProperty(Expression target) {
            // Handle "this.propertyName"
            if (target instanceof FieldAccessExpr) {
                FieldAccessExpr fieldAccess = (FieldAccessExpr) target;
                return fieldAccess.getScope().isThisExpr() && fieldAccess.getNameAsString().equals(propertyName);
            }
            // Handle "propertyName"
            if (target instanceof NameExpr) {
                NameExpr nameExpr = (NameExpr) target;
                return nameExpr.getNameAsString().equals(propertyName);
            }
            return false;
        }
    }
    /**
     * Check if the method call is on ServiceManager.getService(...)
     */
    private boolean isServiceManagerCall(Expression scope) {
        if (scope.isMethodCallExpr()) {
            MethodCallExpr methodCall = scope.asMethodCallExpr();
            return methodCall.getNameAsString().equals("getService")
                    && methodCall.getScope().isPresent()
                    && methodCall.getScope().get().toString().equals("ServiceManager");
        }
        return false;
    }

    /**
     * Recursively check if any argument contains a field of type Cbapalst
     */
    private boolean argumentsContainCbapalst(NodeList<Expression> arguments, MethodCallExpr methodCall) {
        for (Expression argument : arguments) {
            // Get the type of the argument (e.g., "AifArea", "SysArea", etc.)
            String argumentType = getTypeName(argument, methodCall);
            if (argumentType != null && argumentType.endsWith(CBAPALST)) {
                return false;
            }
        }
        for (Expression argument : arguments) {
            // Get the type of the argument (e.g., "AifArea", "SysArea", etc.)
            String argumentType = getTypeName(argument, methodCall);
            if (argumentType != null && classContainsCbapalst(argumentType)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Extract the type name of an argument (e.g., variable or constructor)
     */
    private String getTypeName(Expression argument, MethodCallExpr methodCall) {
        // This implementation assumes variable references or object creations
        if (argument.isNameExpr()) {
            return resolveVariableType(argument.asNameExpr().getNameAsString(), methodCall);
        } else if (argument.isObjectCreationExpr()) {
            return argument.asObjectCreationExpr().getType().getNameAsString();
        }
        return null;
    }

    /**
     * Resolve the fully qualified type of a variable (e.g., "AifArea" -> "com.example.AifArea").
     *
     * @param variableName The name of the variable to resolve.
     * @param methodCall   The AST node where the variable is used (e.g., a MethodCallExpr).
     * @return The fully qualified class name, or null if it cannot be resolved.
     */
    private String resolveVariableType(String variableName, MethodCallExpr methodCall) {
        // Step 1: Find the enclosing method for the given context
        MethodDeclaration enclosingMethod = findEnclosingMethod(methodCall);
        if (enclosingMethod != null) {
            // Search all variable declarations within the enclosing method
            for (VariableDeclarator variable : enclosingMethod.findAll(VariableDeclarator.class)) {
                if (variable.getNameAsString().equals(variableName)) {
                    // Resolve the fully qualified type using imports and package information
                    return resolveFullyQualifiedType(variable.getType().asString(), methodCall);
                }
            }
        }

        // Step 2: Find the enclosing class and check its fields
        ClassOrInterfaceDeclaration enclosingClass = findEnclosingClass(methodCall);
        if (enclosingClass != null) {
            // Search all class-level fields
            for (FieldDeclaration field : enclosingClass.getFields()) {
                for (VariableDeclarator variable : field.getVariables()) {
                    if (variable.getNameAsString().equals(variableName)) {
                        // Resolve the fully qualified type using imports and package information
                        return resolveFullyQualifiedType(variable.getType().asString(), methodCall);
                    }
                }
            }
        }

        // Step 3: If not resolved, print a warning and return null
        System.err.println("Unable to resolve type for variable: " + variableName);
        return null;
    }

    /**
     * Resolve the fully qualified class name of a type using imports and package information.
     *
     * @param typeName    The simple name of the type (e.g., "AifArea").
     * @param contextNode The AST node to derive the package and imports from.
     * @return The fully qualified class name, or the original type name if no package is found.
     */
    private String resolveFullyQualifiedType(String typeName, Node contextNode) {
        // Step 1: Check imports for a match
        CompilationUnit compilationUnit = contextNode.findCompilationUnit().orElse(null);
        if (compilationUnit != null) {
            for (ImportDeclaration importDecl : compilationUnit.getImports()) {
                String importName = importDecl.getNameAsString();
                if (importName.endsWith("." + typeName)) {
                    return importName; // Found a matching import
                }
            }

            // Step 2: If no matching import, check the package declaration
            if (compilationUnit.getPackageDeclaration().isPresent()) {
                String packageName = compilationUnit.getPackageDeclaration().get().getNameAsString();
                return packageName + "." + typeName;
            }
        }

        // Step 3: Fallback to typeName (assume it is in the default package)
        return typeName;
    }

    /**
     * Find the enclosing method of the current variable
     */
    /**
     * Find the enclosing method for the given node (e.g., variable or method call).
     */
    private MethodDeclaration findEnclosingMethod(MethodCallExpr methodCall) {
        return methodCall.findAncestor(MethodDeclaration.class).orElse(null);
    }

    /**
     * Find the enclosing class of the current variable
     */
    private ClassOrInterfaceDeclaration findEnclosingClass(MethodCallExpr methodCall) {
        return methodCall.findAncestor(ClassOrInterfaceDeclaration.class).orElse(null);
    }

    /**
     * Recursively check if a class contains a field of type Cbapalst
     */
    private boolean classContainsCbapalst(String className) {
        File classFile = locateClassFile(className);
        if (classFile == null) {
            return false; // Class file not found
        }

        try {
            // Parse the class file
            CompilationUnit classUnit = StaticJavaParser.parse(classFile);

            // Find fields and recursively check for Cbapalst
            for (FieldDeclaration field : classUnit.findAll(FieldDeclaration.class)) {
                for (VariableDeclarator variable : field.getVariables()) {
                    String fieldType = variable.getType().asString();

                    // If the field is directly of type Cbapalst
                    if (fieldType.equals(CBAPALST)) {
                        return true;
                    }

                    // Recursively check nested fields
                    if (classContainsCbapalst(fieldType)) {
                        return true;
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Locate the file corresponding to a given class name
     */
    private File locateClassFile(String className) {
        // Replace dots in the class name with file separators (e.g., "com.example.AifArea")
        String relativePath = className.replace('.', File.separatorChar) + ".java";

        // Check if the file exists in the source directory
        File file = new File(SOURCE_DIRECTORY, relativePath);
        return file.exists() ? file : null;
    }
}
