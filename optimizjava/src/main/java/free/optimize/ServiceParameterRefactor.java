package free.optimize;

import com.github.javaparser.*;
import com.github.javaparser.ast.*;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.type.*;

import java.io.*;
import java.util.*;

/**
 * ClassPropertyReferenceAnalyzer: Analyzes and processes references to class properties within Java files.
 * This class parses Java source files, identifies references to specific class properties,
 * and generates new parameter classes for better encapsulation.
 *
 * @version 4.0
 */
public class ServiceParameterRefactor {

    private static final String SERVICE_MANAGER_GET_SERVICE = "ServiceManager.getService(";
    private final Map<String, ApiBizParameter> apiBizParameterMap = new HashMap<>();

    /**
     * Main method to analyze and generate required parameter classes.
     *
     * @param classpath      The root classpath of the project.
     * @param fileAPath      Path to the file (A) being analyzed.
     * @param oldApiClass     Name of the class (B) whose properties are referenced.
     * @param fullClassName  Fully qualified name of class (B).
     * @param apiPath        Path to save the generated API classes.
     * @throws IOException If file operations fail.
     */
    public void analyzeAndGenerate(
            String classpath, String fileAPath, String oldApiClass, String fullClassName, String apiPath
    ) throws IOException {
        // Step 1: Extract properties of oldApiClass
        List<String> properties = ClassPropertiesExtractor.getClassProperties(classpath, fullClassName);

        // Step 2: Analyze references to properties of old ApiClass in file A， and recursive with service call
        CompilationUnit mainUnit = analyzeClassPropertyReferences(classpath, fileAPath, oldApiClass, properties);
        String mainServiceName = fileAPath.substring(fileAPath.lastIndexOf(File.separator) + 1, fileAPath.indexOf("."));

        // Step 3: Process each referenced API parameter and generate parameter classes
        for (String key : apiBizParameterMap.keySet()) {
            ApiBizParameter apiBizParameter = apiBizParameterMap.get(key);
            List<String> parameters = apiBizParameter.getParameters();

//            if (parameters.isEmpty()) continue; // Skip if no parameters are found

            // Generate parameter class for each API business parameter
            CompilationUnit compilationUnit = ClassPropertiesExtractor.getCompilationUnit(classpath, fullClassName);

            // Filter unused fields
            filterFieldsByParameters(compilationUnit, parameters);

            // Generate new parameter class with methods
            String newClassName = extractClassNameFromPath(key);
            String newClassParamsName = newClassName + "Params";
            ClassOrInterfaceDeclaration classDeclaration = compilationUnit
                    .findAll(ClassOrInterfaceDeclaration.class)
                    .get(0);
            classDeclaration.setName(newClassParamsName);

            generateGetMethods(apiBizParameter, classDeclaration);

            // Save the generated class
            saveClassToFile(compilationUnit, new File(apiPath, newClassParamsName + ".java"));
        }

        // Replace references in the original file (file A)
        replaceClassReferences(mainUnit, oldApiClass, mainServiceName+"Params");

        // Output the final updated file for debugging
        saveClassToFile(mainUnit,new File(fileAPath));
    }

    /**
     * Analyzes references to class properties in a file.
     *
     * @param classpath   The root classpath of the project.
     * @param fileAPath   Path to the file being analyzed.
     * @param oldApiClass  Name of the old ApiClasseing analyzed.
     * @param properties  List of properties to analyze.
     * @return The CompilationUnit of the analyzed file.
     * @throws IOException If the file cannot be read.
     */
    private CompilationUnit analyzeClassPropertyReferences(
            String classpath, String fileAPath, String oldApiClass, List<String> properties
    ) throws IOException {
        try (FileInputStream fis = new FileInputStream(fileAPath)) {
            CompilationUnit compilationUnit = StaticJavaParser.parse(fis);
            ApiBizParameter apiBizParameter = getOrCreateApiBizParameter(fileAPath);

            String field = findMatchingField(compilationUnit, oldApiClass);
            if (field == null) return compilationUnit;

            for (ExpressionStmt stmt : compilationUnit.findAll(ExpressionStmt.class)) {
                handleExpressionStatement(stmt, field, properties, classpath, fileAPath, oldApiClass, apiBizParameter);
            }

            return compilationUnit;
        }
    }

    /**
     * Handles a single expression statement, checking for method calls and field references.
     */
    private void handleExpressionStatement(
            ExpressionStmt stmt, String field, List<String> properties, String classpath,
            String fileAPath, String oldApiClass, ApiBizParameter apiBizParameter
    ) {
        MethodAndParamIndex methodCall = getMethodCallWithField(stmt, field);
        if (methodCall != null) {
            processMethodCall(methodCall, field, classpath, fileAPath, oldApiClass, apiBizParameter,properties);
        }

        for (FieldAccessExpr fieldAccess : stmt.findAll(FieldAccessExpr.class)) {
            if (properties.contains(fieldAccess.getNameAsString())) {
                apiBizParameter.addParameter(fieldAccess.getNameAsString());
            }
        }
    }

    /**
     * Processes a method call containing a reference to a specific field.
     */
    private void processMethodCall(
            MethodAndParamIndex methodCall, String field, String classpath, String fileAPath,
            String oldApiClass, ApiBizParameter apiBizParameter,
            List<String> properties) {
        String stmtContent = methodCall.expr.toString();
        if (!stmtContent.contains(SERVICE_MANAGER_GET_SERVICE)) return;

        String subClsName = extractClassNameFromServiceManagerCall(stmtContent);
        if (subClsName == null) return;

        File subFilePath = new File(classpath, subClsName.replace(".", File.separator) + ".java");
        try {
            String subFilePathAbsolutePath = subFilePath.getAbsolutePath();
            CompilationUnit subUnit = analyzeClassPropertyReferences(classpath, subFilePathAbsolutePath, oldApiClass, properties);
            String subServiceName = subClsName.substring(subClsName.lastIndexOf(".") + 1);
//            allServices.put(subServiceName,subUnit);
            String newParamClassName = subServiceName + "Params";
            replaceClassReferences(subUnit, oldApiClass, newParamClassName);
            saveClassToFile(subUnit,subFilePath);
            MethodCallExpr replacement = new MethodCallExpr(new NameExpr(field), "get" + newParamClassName);
            methodCall.expr.setArgument(methodCall.paramIndex, replacement);

            ApiBizParameter subParam = apiBizParameterMap.get(subFilePathAbsolutePath);
            if (subParam != null) {
                apiBizParameter.addSubBiz(subFilePathAbsolutePath);
                for(String param:subParam.getParameters())
                    apiBizParameter.addParameter(param);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Filters fields in the compilation unit based on the provided parameter list.
     */
    private void filterFieldsByParameters(CompilationUnit compilationUnit, List<String> parameters) {
        // Retrieve all field declarations from the compilation unit
        List<FieldDeclaration> fields = compilationUnit.findAll(FieldDeclaration.class);

        // Iterate through each field declaration
        for (FieldDeclaration field : fields) {
            boolean keepField = false; // Flag to determine whether to keep this field

            // Iterate through the variables in the field declaration
            for (VariableDeclarator variable : field.getVariables()) {
                // Check if the variable's name exists in the list of parameters
                if (parameters.contains(variable.getNameAsString())) {
                    keepField = true; // Mark the field to be kept
                    break; // Exit the loop as we found a match
                }
            }

            // If no variables in this field match the parameters, remove the field
            if (!keepField) {
                field.remove();
            }
        }
    }

    /**
     * Generates `get` methods for each sub-business in the API parameter.
     */
    private void generateGetMethods(ApiBizParameter apiBizParameter, ClassOrInterfaceDeclaration classDeclaration) {
        for (String subBiz : apiBizParameter.getSubBizNames()) {
            String subBizName = extractClassNameFromPath(subBiz) + "Params";

            MethodDeclaration method = classDeclaration.addMethod("get" + subBizName, Modifier.Keyword.PUBLIC);
            method.setType(new ClassOrInterfaceType(null, subBizName));

            BlockStmt body = new BlockStmt();
            String variableName = "the" + subBizName;

            body.addStatement(new AssignExpr(
                    new VariableDeclarationExpr(new ClassOrInterfaceType(null, subBizName), variableName),
                    new ObjectCreationExpr(null, new ClassOrInterfaceType(null, subBizName), new NodeList<>()),
                    AssignExpr.Operator.ASSIGN
            ));

            ApiBizParameter subApi = getOrCreateApiBizParameter(subBiz);
            for (String param : subApi.getParameters()) {
                body.addStatement(new AssignExpr(
                        new NameExpr(variableName + "." + param),
                        new NameExpr(param),
                        AssignExpr.Operator.ASSIGN
                ));
            }

            body.addStatement(new ReturnStmt(new NameExpr(variableName)));
            method.setBody(body);
        }
    }

    /**
     * Replaces all references to the old class name with the new class name in the CompilationUnit.
     */
    private void replaceClassReferences(CompilationUnit compilationUnit, String oldClassName, String newClassName) {
        for (ImportDeclaration importDecl : compilationUnit.getImports()) {
            if (importDecl.getNameAsString().endsWith("." + oldClassName)) {
                importDecl.setName(importDecl.getNameAsString().replace(oldClassName, newClassName));
            }
        }

        for (ClassOrInterfaceType type : compilationUnit.findAll(ClassOrInterfaceType.class)) {
            if (type.getNameAsString().equals(oldClassName)) {
                type.setName(newClassName);
            }
        }

        for (NameExpr nameExpr : compilationUnit.findAll(NameExpr.class)) {
            if (nameExpr.getNameAsString().equals(oldClassName)) {
                nameExpr.setName(newClassName);
            }
        }
    }

    /**
     * Extracts the class name from a service manager call statement.
     */
    private String extractClassNameFromServiceManagerCall(String stmtContent) {
        int start = stmtContent.indexOf(SERVICE_MANAGER_GET_SERVICE) + SERVICE_MANAGER_GET_SERVICE.length();
        int end = stmtContent.indexOf(")", start);
        if (start >= 0 && end > start && stmtContent.indexOf(".class") != -1) {
            return stmtContent.substring(start, end).replace(".class", "");
        }
        return null;
    }

    /**
     * Extracts the class name from a file path.
     */
    private String extractClassNameFromPath(String path) {
        int idx = path.lastIndexOf(File.separator);
        return path.substring(idx + 1, path.lastIndexOf("."));
    }

    /**
     * Saves the updated class to a file.
     */
    private void saveClassToFile(CompilationUnit compilationUnit, File file) {
        try (FileWriter writer = new FileWriter(file)) {
            writer.write(compilationUnit.toString());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private ApiBizParameter getOrCreateApiBizParameter(String bizName) {
        // Check if the bizName exists in the map
        ApiBizParameter param = apiBizParameterMap.get(bizName);
        if (param == null) {
            // Create a new ApiBizParameter if it doesn't exist
            param = new ApiBizParameter();
            param.setBizId(bizName);
            apiBizParameterMap.put(bizName, param);
        }
        return param;
    }

    private MethodAndParamIndex getMethodCallWithField(ExpressionStmt stmt, String fieldName) {
        for (MethodCallExpr methodCall : stmt.findAll(MethodCallExpr.class)) {
            for (int i = 0; i < methodCall.getArguments().size(); i++) {
                if (methodCall.getArguments().get(i) instanceof NameExpr nameExpr
                        && nameExpr.getNameAsString().equals(fieldName)) {
                    return new MethodAndParamIndex(methodCall, i);
                }
            }
        }
        return null;
    }

    private String findMatchingField(CompilationUnit compilationUnit, String targetType) {
        for (ClassOrInterfaceDeclaration clazz : compilationUnit.findAll(ClassOrInterfaceDeclaration.class)) {
            for (FieldDeclaration field : clazz.getFields()) {
                for (VariableDeclarator variable : field.getVariables()) {
                    if (variable.getType().asString().equals(targetType)) {
                        return variable.getNameAsString();
                    }
                }
            }
        }
        return null;
    }

    private record MethodAndParamIndex (MethodCallExpr expr,int paramIndex){
    }

    public static void main(String[] args) {
        try {
            String classpath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java";

            String fileAPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/onbb/cbl/Gsa01060.java";
//            String fileAPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/kbcf/cbl/Cshtra0.java";
            String oldApiClass = "Cbapalst";
            String fullClassName = "cbod.java.models.ccbmain.copy.Cbapalst";
            String apiPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/models/ccbmain/copy";

            ServiceParameterRefactor analyzer = new ServiceParameterRefactor();
            analyzer.analyzeAndGenerate(classpath, fileAPath, oldApiClass, fullClassName, apiPath);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}