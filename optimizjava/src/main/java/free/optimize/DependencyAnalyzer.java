package free.optimize;

import com.github.javaparser.*;
import com.github.javaparser.ast.*;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.nodeTypes.NodeWithSimpleName;
import com.github.javaparser.ast.stmt.*;
import free.servpp.mustache.CodeFormator;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.model.BaseSection;
import io.swagger.v3.oas.models.OpenAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;

/**
 * ClassPropertyReferenceAnalyzer: Analyzes and processes references to class properties within Java files.
 * This class parses Java source files, identifies references to specific class properties,
 * and generates new parameter classes for better encapsulation.
 *
 * @version 4.0
 */
public class DependencyAnalyzer {

    private static final String SERVICE_MANAGER_GET_SERVICE = "ServiceManager.getService(";

    private List<String> visitedFiles = new ArrayList<>();
    private List<ApiField> usedProperties = new ArrayList<>();
    private List<String> services = new ArrayList<>();
    private Map<String,String> handlerFields = new HashMap<>();
    /**
     * Main method to analyze and generate required parameter classes.
     *
     * @param classpath      The root classpath of the project.
     * @param fileAPath      Path to the file (A) being analyzed.
     * @param classBName     Name of the class (B) whose properties are referenced.
     * @param apiPath        Path to save the generated API classes.
     * @throws IOException If file operations fail.
     */
    public void analyzeAndGenerate(String classpath, String ignoreClass,String fileAPath, String classBName, String apiPath) throws IOException {
        // Step 1: Extract properties of class B
        Map<String,List<ApiField>> properties =
                new BasicAttributesExtractor(classpath,ignoreClass).extractBasicAttributes(new File(apiPath, classBName+".java"));

        // Step 2: Analyze references to properties of class B in file A
        analyzeClassPropertyReferences(classpath, fileAPath, classBName, properties);
    }

    /**
     * Analyzes references to class properties in a file.
     *
     * @param classpath   The root classpath of the project.
     * @param fileAPath   Path to the file being analyzed.
     * @param classBName  Name of the class being analyzed.
     * @param properties  List of properties to analyze.
     * @return The CompilationUnit of the analyzed file.
     * @throws IOException If the file cannot be read.
     */
    private void analyzeClassPropertyReferences(String classpath, String fileAPath,
                                                String classBName, Map<String,List<ApiField>> properties) throws IOException {
        if(!visitedFiles.contains(fileAPath)) {
            visitedFiles.add(fileAPath);
            try (FileInputStream fis = new FileInputStream(fileAPath)) {
                CompilationUnit compilationUnit = StaticJavaParser.parse(fis);
                findHandlerFields(compilationUnit);
                for (ExpressionStmt stmt : compilationUnit.findAll(ExpressionStmt.class)) {
                    handleExpressionStatement(stmt, properties, classpath, fileAPath, classBName);
                }
                handleFieldAccess(compilationUnit,properties);
            }
        }
    }

    private void findHandlerFields(CompilationUnit compilationUnit) {
        List<ClassOrInterfaceDeclaration> innerClasses = compilationUnit.findAll(ClassOrInterfaceDeclaration.class);
        for(ClassOrInterfaceDeclaration classOrInterfaceDeclaration : innerClasses) {
            if(classOrInterfaceDeclaration.getNameAsString().endsWith("Handler")) {
                List<FieldDeclaration> fields = classOrInterfaceDeclaration.getFields();
                for(FieldDeclaration fieldDeclaration:fields){
                    handlerFields.put(fieldDeclaration.getVariables().get(0).getNameAsString(),fieldDeclaration.getElementType().asString());
                }
            }
        }
    }

    private void handleFieldAccess(CompilationUnit compilationUnit, Map<String,List<ApiField>> properties){
        List<FieldAccessExpr> fieldAccessExprs = compilationUnit.findAll(FieldAccessExpr.class);
        handleFieldAccessExprs(null,properties,fieldAccessExprs);
    }

    /**
     * Handles a single expression statement, checking for method calls and field references.
     */
    private void handleExpressionStatement(
            ExpressionStmt stmt, Map<String, List<ApiField>> properties, String classpath,
            String fileAPath, String classBName) throws IOException {
        List<FieldAccessExpr> fieldAccessExprs = stmt.findAll(FieldAccessExpr.class);
        handleFieldAccessExprs(stmt, properties, fieldAccessExprs);

        for (MethodCallExpr methodCall : stmt.findAll(MethodCallExpr.class)) {
            String stmtContent = methodCall.toString();
            if (!stmtContent.contains(SERVICE_MANAGER_GET_SERVICE)) continue;
            processMethodCall(methodCall, classpath, fileAPath, classBName, properties);
        }
    }

    private void handleFieldAccessExprs(ExpressionStmt stmt, Map<String, List<ApiField>> properties, List<FieldAccessExpr> fieldAccessExprs) {
        for (FieldAccessExpr fieldAccess : fieldAccessExprs) {
            String nameAsString = fieldAccess.getNameAsString();
            List<ApiField> props = properties.get(nameAsString);
            String scope = fieldAccess.getScope().toString();
            int index = scope.indexOf(".");
            String firstScope = index == -1 ? scope : scope.substring(0,index);
            String firstScopeClass = handlerFields.get(firstScope);
            if (firstScopeClass != null && props != null && props.size() > 0) {
                if(props.size() > 1)
                    System.out.println("duplicate prop " + nameAsString);
                ApiField apiField = props.get(0);
                boolean isInApiFieldScope = false;
                for(BasicAttributesExtractor.NameAndClass nameAndClass:apiField.getNameAndClassList()){
                    isInApiFieldScope = nameAndClass.className().indexOf(firstScopeClass) != -1;
                    if(isInApiFieldScope)
                        break;
                }
                if(isInApiFieldScope) {
                    if (!usedProperties.contains(apiField))
                        usedProperties.add(apiField);
                    if (!apiField.isIn() || !apiField.isOut()) {
                        if (stmt != null) {
                            for (AssignExpr expr : stmt.findAll(AssignExpr.class)) {
                                Expression theTarget = expr.getTarget();
                                NodeWithSimpleName target = null;
                                if (theTarget instanceof NodeWithSimpleName) {
                                    target = (NodeWithSimpleName) theTarget;
                                } else if (theTarget instanceof ArrayAccessExpr) {
                                    target = (NodeWithSimpleName) ((ArrayAccessExpr) theTarget).getName();
                                }
                                if (target.getNameAsString().equals(nameAsString)) {
                                    apiField.setOut(true);
                                    apiField.setOutStmt(stmt.toString());
                                } else {
                                    apiField.setIn(true);
                                    apiField.setInStmt(stmt.toString());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Processes a method call containing a reference to a specific field.
     */
    private void processMethodCall(
            MethodCallExpr methodCall, String classpath, String fileAPath,
            String classBName, Map<String,List<ApiField>> properties) throws IOException {
        String stmtContent = methodCall.toString();
        String subClsName = extractClassNameFromServiceManagerCall(stmtContent);
        if (subClsName == null) return;

        File subFilePath = new File(classpath, subClsName.replace(".", File.separator) + ".java");
        if(subFilePath.exists()) {
            if(!services.contains(subClsName)) {
                services.add(subClsName);
                String subFilePathAbsolutePath = subFilePath.getAbsolutePath();
                analyzeClassPropertyReferences(classpath, subFilePathAbsolutePath, classBName, properties);
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
    public MustacheListenerImpl createMustacheListener(URL url) throws URISyntaxException, IOException {
        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
        mustacheCompiler.compileAntlr4(null);
        MustacheListenerImpl listener = new MustacheListenerImpl(url.toURI());
        mustacheCompiler.workListener(listener);
        return listener;
    }
    public void writeToFile(File outputFilePath, String content) {
        File targetFile = outputFilePath;
        targetFile.getParentFile().mkdirs();  // Create directories if they don't exist

        try (FileWriter writer = new FileWriter(targetFile)) {
            writer.write(content);
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void addBeans(File xmlFile) {
        try {
            // 1. parse XML file
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.parse(xmlFile);

            // 2. Get Root（<beans>）
            Element root = document.getDocumentElement();

            for(String service:services) {
                String id = service.substring(service.lastIndexOf(".") +1);
                if(document.getElementById(id) != null)
                    continue;

                // 3. create new  <bean> element
                Element newBean = document.createElement("bean");
                newBean.setAttribute("id", id);
                newBean.setAttribute("class", service);

                // 4. add new element to root
                root.appendChild(newBean);
            }

            // 5. Save XML file
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            DOMSource source = new DOMSource(document);
            StreamResult result = new StreamResult(xmlFile);
            transformer.transform(source, result);

            System.out.println("New bean added successfully!");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public static void main(String[] args) throws URISyntaxException {
        try {
            String classpath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java";
            String fileAPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/onbb/cbl/Gsa01060.java";
            String classBName = "Gsa01060Params";
            String apiPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/java/cbod/java/models/ccbmain/copy";
            String yamlPath = "/Users/lidong/test/cbodjava/generated-sources/cbod/src/main/resources/";
            String ignoreClass = "Cbapalst";

            DependencyAnalyzer analyzer = new DependencyAnalyzer();
            analyzer.analyzeAndGenerate(classpath, ignoreClass,fileAPath, classBName, apiPath);

//            for(int i = 0;i<analyzer.usedProperties.size();i++) {
//                System.out.println(i+":"+analyzer.usedProperties.get(i));
//            }
            OpenAPI openAPI = ApiFieldToOpenApiGenerator.generateOpenApiWithPaths("Gsa01060",analyzer.usedProperties);
            ApiFieldToOpenApiGenerator.saveAsYaml(openAPI, yamlPath + "/Gsa01060.yaml");

            Object root = new Object() {
                String packageName = "cbod.java.models.setters";
                String serviceName = "Gsa01060";
                String servicePath = "cbod.java.onbb.cbl";
                String method = "Put";
                String openApiModelPath = "free.cbod.model";
                String apiPath = "cbod.java.models.ccbmain.copy";
                String setterName = serviceName.toLowerCase() + method;
                List<ApiField> properties = analyzer.usedProperties;
            };
            URL url = MappingMustacheWriter.class.getResource("/apiSetter.mustache");
            MappingMustacheWriter writer = new MappingMustacheWriter(
                    url.toURI(), root, false);
            MustacheListenerImpl listener = analyzer.createMustacheListener(url);
            writer.write(listener.getTemplate(), BaseSection.SectionType.Normal);
            StringBuffer sb = writer.getOutText();
            analyzer.writeToFile(new File(classpath,"/cbod/java/models/setters/gsa01060Put_Setter.java"),
                    CodeFormator.formatCode(sb.toString()));
            analyzer.addBeans(new File(yamlPath,"applicationContext.xml"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}