package free.cobol2java.parser;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * @author lidong@date 2024-09-30@version 1.0
 */
public class TopCompiler {
    static Stack<String> cobolStack = new Stack<>();
    static Map<String, CobolCompiler> copybookStruct = new HashMap<>();
    public static void enterCobol(String cobolName){
        cobolStack.push(cobolName);
        copybookStruct.put(cobolName,new CobolCompiler(cobolName));
    }

    public static CobolCompiler getCobolCompiler(String name){
        return copybookStruct.get(name);
    }

    public static CobolCompiler currentCompiler(){
        return getCobolCompiler(currentCobol());
    }
    public static String currentCobol(){
        return cobolStack.peek();
    }

    public static String exitCobol(){
        return cobolStack.pop();
    }
}
