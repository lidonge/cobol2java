package free.cobol2java;

import java.lang.reflect.Field;
import java.lang.reflect.InaccessibleObjectException;
import java.util.*;

public class ObjectTreePrinter {

    private Set<Object> visited = new HashSet<>();
    private Object root;

    public static void main(String[] args) {
        // Example usage
        NestedClass nested = new NestedClass(42, "Nested");
        ExampleClass example = new ExampleClass("Root", nested, List.of(1, 2, 3), Map.of("key1", "value1", "key2", nested));
        ObjectTreePrinter printer = new ObjectTreePrinter();
        printer.printObjectTree(example);
    }

    /**
     * Prints the structure of the given object in a tree-like format.
     *
     * @param obj The object to print
     */
    public void printObjectTree(Object obj) {
        visited.clear(); // Clear the visited set before starting the print
        if (obj == null) {
            System.out.println("null");
            return;
        }
        root = obj;
        printObjectTree(obj, "");
    }

    private void printObjectTree(Object obj, String indent) {
        if (obj == null) {
            System.out.println(indent + "null");
            return;
        }

        if (visited.contains(obj)) {
//            System.out.println(indent + "[Circular Reference]");
            return;
        }

        visited.add(obj);

        Class<?> clazz = obj.getClass();

        List<Field> fields = getAllFields(clazz);
        for (Field field : fields) {
            try {
                if (java.lang.reflect.Modifier.isStatic(field.getModifiers()))
                    continue;
                field.setAccessible(true);
                Object value = field.get(obj);
                if(visited.contains(value))
                    continue;

                if(value == null)
                    continue;
                String fieldName = field.getName();
                if("ctx".equals(fieldName)
                        || "lines".equals(fieldName)
                        || "_listeners".equals(fieldName)
                        || "compilationUnit".equals(fieldName)
                        || "asgElementRegistry".equals(fieldName)
                        || "program".equals(fieldName)
                        || "subValueStmts".equals(fieldName)
                        || fieldName.indexOf("Container") != -1
                        || fieldName.indexOf("Successor") != -1
                        || fieldName.indexOf("SymbolTable") != -1
                        || fieldName.startsWith("_"))
                    continue;
                String typeName = field.getType().getSimpleName();
                if(typeName.indexOf("[]") != -1)
                    continue;
                System.out.print(indent + "  " + typeName +" "+ fieldName + " = ");
                if (isPrimitiveOrWrapper(value)) {
                    System.out.println(value);
                } else if (value instanceof String) {
                    System.out.println("\"" + value + "\"");
                } else if (value instanceof List && ((List<?>) value).size() != 0) {
                    System.out.println(((List<?>) value).size());
                    printList((List<?>) value, indent + "  ");
                }
//                else if (value instanceof Map && ((Map<?, ?>) value).size() != 0) {
//                    System.out.println(((Map<?, ?>) value).size());
//                    printMap((Map<?, ?>) value, indent + "  ");
//                }
//                else if (value.getClass().isArray() && ((Object[])value).length != 0) {
//                    System.out.println(((Object[])value).length);
//                    printArray(value, indent + "  ");
//                }
                else {
                    System.out.println("[Object]");
                    printObjectTree(value, indent + "  ");
                }
            } catch (IllegalAccessException e) {
                System.out.println(indent + "  Error accessing field: " + e.getMessage());
            } catch (InaccessibleObjectException e){}
        }
    }

    private List<Field> getAllFields(Class clazz){
        List<Field> fields = new ArrayList<>();
        do{
            Field[] theFields = clazz.getDeclaredFields();
            for(Field field:theFields)
                fields.add(field);
            clazz = clazz.getSuperclass();
        }while(!clazz.isInstance(Object.class));
        return fields;
    }
    private boolean isPrimitiveOrWrapper(Object obj) {
        return obj.getClass().isPrimitive() || obj instanceof Number || obj instanceof Boolean;
    }

    private void printArray(Object array, String indent) {
        int length = java.lang.reflect.Array.getLength(array);
        for (int i = 0; i < length; i++) {
            Object element = java.lang.reflect.Array.get(array, i);
            if(visited.contains(element))
                continue;
            System.out.print(indent + "[" + i + "] = ");
            if (element == null) {
                System.out.println("null");
            } else if (isPrimitiveOrWrapper(element)) {
                System.out.println(element);
            } else {
                System.out.println("[Object]");
                printObjectTree(element, indent);
            }
        }
    }

    private void printList(List<?> list, String indent) {
        for (int i = 0; i < list.size(); i++) {
            Object element = list.get(i);
            System.out.print(indent + "[" + i + "] = ");
            if (element == null) {
                System.out.println("null");
            } else if (isPrimitiveOrWrapper(element)) {
                System.out.println(element);
            } else {
//                System.out.println("[Object]");
                printObjectTree(element, indent);
            }
        }
    }

    private void printMap(Map<?, ?> map, String indent) {
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Object key = entry.getKey();
            Object value = entry.getValue();
            if(visited.contains(value))
                continue;
            System.out.print(indent + "[" + key + "] = ");
            if (value == null) {
                System.out.println("null");
            } else if (isPrimitiveOrWrapper(value)) {
                System.out.println(value);
            } else {
//                System.out.println("[Object]");
                printObjectTree(value, indent);
            }
        }
    }
}

// Example classes for demonstration
class ExampleClass {
    private String name;
    private NestedClass nested;
    private List<Integer> numbers;
    private Map<String, Object> map;

    public ExampleClass(String name, NestedClass nested, List<Integer> numbers, Map<String, Object> map) {
        this.name = name;
        this.nested = nested;
        this.numbers = numbers;
        this.map = map;
    }
}

class NestedClass {
    private int value;
    private String description;

    public NestedClass(int value, String description) {
        this.value = value;
        this.description = description;
    }
}