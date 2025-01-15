package free.optimize;

/**
 * @author lidong@date 2024-12-12@version 1.0
 */
import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Name;
import com.github.javaparser.ast.type.Type;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;

public class BasicAttributesExtractor {
    private String clsPath;
    private String ignoreClass;

    public record NameAndClass(String name, String className){

    };
    public BasicAttributesExtractor(String clsPath, String ignoreClass) {
        this.clsPath = clsPath;
        this.ignoreClass = ignoreClass;
    }

    public Map<String, List<ApiField>> extractBasicAttributes(File javaFile) throws IOException {
        // Parse the Java file
        CompilationUnit compilationUnit = StaticJavaParser.parse(javaFile);

        // Map to store the result
        Map<String, List<ApiField>> basicAttributesMap = new HashMap<>();

        // Find all class declarations
        List<ClassOrInterfaceDeclaration> classDeclarations = compilationUnit.findAll(ClassOrInterfaceDeclaration.class);

        for (ClassOrInterfaceDeclaration clazz : classDeclarations) {
            String className = clazz.getNameAsString();
            processClass(clazz, className, basicAttributesMap, "", new Stack());
        }

        return basicAttributesMap;
    }

    private void processClass(ClassOrInterfaceDeclaration clazz,
                                     String currentQualifiedName,
                                     Map<String, List<ApiField>> map,
                                     String prefix,Stack<NameAndClass> classStack) throws IOException {
        List<FieldDeclaration> fields = clazz.getFields();
        for (FieldDeclaration field : fields) {
            List<VariableDeclarator> variables = field.getVariables();
            for (VariableDeclarator variable : variables) {
                String fieldName = variable.getNameAsString();
                Type fieldType = variable.getType();

                // Check if it's a primitive type
                String curFullName = prefix.isEmpty() ? fieldName : prefix + "." + fieldName;
                if (fieldType.isPrimitiveType() || isWrapperType(fieldType)) {
                    String qualifiedName = curFullName;
                    if (!map.containsKey(fieldName)) {
                        map.put(fieldName, new ArrayList<>());
                    }
                    ApiField apiField = new ApiField(qualifiedName, fieldType, field.getAnnotations());
                    apiField.setNameAndClassList(classStack);
                    map.get(fieldName).add(apiField);
                } else {
                    // If it's not a primitive type, recursively process its class definition
                    if (fieldType.isClassOrInterfaceType()) {
                        ClassOrInterfaceDeclaration fieldClass = findClassDeclaration(fieldType.asString());
                        if (fieldClass != null) {
                            String packageName = ((CompilationUnit) fieldClass.getParentNode().get()).getPackageDeclaration().get().getName().asString();
                            String fullClassName = packageName + "." + fieldType.asString();
                            classStack.push(new NameAndClass(curFullName,fullClassName));
                            processClass(fieldClass,
                                    currentQualifiedName + "." + fieldName,
                                    map,
                                    curFullName, classStack);
                            classStack.pop();
                        }
                    }
                }
            }
        }
    }

    private boolean isWrapperType(Type type) {
        // List of wrapper types in Java
        List<String> wrapperTypes = Arrays.asList(
                "Integer", "Long", "Double", "Float", "Short", "Byte", "Boolean", "Character", "String", "Object"
        );
        if (type.isClassOrInterfaceType()) {
            String typeName = type.asClassOrInterfaceType().getNameAsString();
            for (String wrapperType : wrapperTypes) {
                if (wrapperType.equals(typeName)) {
                    return true;
                }
            }
        }
        return false;
    }

    private ClassOrInterfaceDeclaration findClassDeclaration(String className) throws IOException {
//        File file = new File(clsPath, className + ".java");
        if(!className.equals(ignoreClass)) {
            File file = searchFile(new File(clsPath), className + ".java");
            if (file != null) {
                CompilationUnit compilationUnit = StaticJavaParser.parse(file);

                // Find all class declarations
                List<ClassOrInterfaceDeclaration> classDeclarations = compilationUnit.findAll(ClassOrInterfaceDeclaration.class);

                return classDeclarations.get(0);
            }
        }
        return null;
    }
    private File searchFile(File dir, String targetFileName) {
        // 获取目录下的所有文件和子目录
        File[] files = dir.listFiles();
        File ret = null;
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    ret = searchFile(file, targetFileName);
                } else if (file.isFile() && file.getName().equals(targetFileName)) {
                    ret = file;
                }
                if(ret != null)
                    break;
            }
        }
        return ret;
    }
    public static void main(String[] args) throws Exception {
        String path = "/Users/lidong/test/cbodjava/src/main/java/cbod/java/models/ccbmain/copy";
        String clsPatn = "/Users/lidong/test/cbodjava/src/main/java";
        File file = new File(path, "Gsa01060Params.java");
        Map<String, List<ApiField>> basicAttributes = new BasicAttributesExtractor(clsPatn,"Cbapalst").extractBasicAttributes(file);

        for (Map.Entry<String, List<ApiField>> entry : basicAttributes.entrySet()) {
            System.out.println("Instance Name: " + entry.getKey());
            System.out.println("Qualified Names: " + entry.getValue());
        }
    }
}