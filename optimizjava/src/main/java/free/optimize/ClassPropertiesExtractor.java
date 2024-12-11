package free.optimize;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * A utility class for extracting properties (fields) of a Java class from its source file.
 * This uses JavaParser to parse the source code and retrieve the fields of a class.
 *
 * @author lidong
 * @date 2024-12-06
 * @version 1.0
 */
public class ClassPropertiesExtractor {

    /**
     * Extracts the list of properties (fields) of a specified class from its source file.
     *
     * @param classpath     The path to the project source code.
     * @param fullclassname The fully qualified name of the class (e.g., com.example.MyClass).
     * @return A list of property names (field names) of the class.
     * @throws IOException If the source file cannot be found or read.
     */
    public static List<String> getClassProperties(String classpath, String fullclassname) throws IOException {
        // Parse the Java source file into a CompilationUnit object
        CompilationUnit compilationUnit = getCompilationUnit(classpath, fullclassname);

        // List to hold the names of the properties (fields)
        List<String> properties = new ArrayList<>();

        // Find all class or interface declarations in the source file
        List<ClassOrInterfaceDeclaration> classes = compilationUnit.findAll(ClassOrInterfaceDeclaration.class);

        for (ClassOrInterfaceDeclaration clazz : classes) {
            // Retrieve all field declarations within the class
            List<FieldDeclaration> fields = clazz.getFields();

            // Iterate through each field and extract the variable names
            for (FieldDeclaration field : fields) {
                for (VariableDeclarator variable : field.getVariables()) {
                    // Add each variable's name to the properties list
                    properties.add(variable.getNameAsString());
                }
            }
            // Break after processing the first matching class (assumes one class per file)
            break;
        }

        return properties;
    }

    /**
     * Loads and parses the source file for a specified class into a CompilationUnit.
     *
     * @param classpath     The path to the project source code.
     * @param fullclassname The fully qualified name of the class (e.g., com.example.MyClass).
     * @return A CompilationUnit representing the parsed Java source file.
     * @throws IOException If the source file cannot be found or read.
     */
    public static CompilationUnit getCompilationUnit(String classpath, String fullclassname) throws IOException {
        // Construct the path to the source file
        String filePath = classpath + "/" + fullclassname.replace(".", "/") + ".java";

        // Verify that the file exists
        File file = new File(filePath);
        if (!file.exists()) {
            throw new IOException("File not found: " + filePath);
        }

        // Use a FileInputStream to read the file and parse it with JavaParser
        FileInputStream in = null;
        try {
            in = new FileInputStream(file);
            return StaticJavaParser.parse(in);
        } finally {
            if (in != null) {
                in.close(); // Ensure the file stream is closed to prevent resource leaks
            }
        }
    }

    /**
     * Main method for testing the utility.
     * Demonstrates how to extract properties from a Java class source file.
     *
     * @param args Command-line arguments (not used).
     */
    public static void main(String[] args) {
        try {
            // Example source code path and fully qualified class name
            String classpath = "/Users/lidong/test/cbodjava/src/main/java";
            String fullclassname = "cbod.java.models.ccbmain.copy.Cbapalst";

            // Extract the properties of the specified class
            List<String> properties = getClassProperties(classpath, fullclassname);

            // Print the properties
            System.out.println("Properties of class " + fullclassname + ":");
            for (String property : properties) {
                System.out.println(property);
            }
        } catch (IOException e) {
            // Print the error message and stack trace if an exception occurs
            e.printStackTrace();
        }
    }
}