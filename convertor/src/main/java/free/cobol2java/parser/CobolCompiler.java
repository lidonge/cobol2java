package free.cobol2java.parser;

import free.cobol2java.context.IExprBaseContext;

import java.util.*;

/**
 * @author lidong@date 2024-09-30@version 1.0
 */
public class CobolCompiler {
    private String cobolName;
    private List<String> includes = new ArrayList<>();
    private Stack<String> copybookStack = new Stack<>();
    private Map<String,List<String>> copybookIncludes = new HashMap<>();

    public CobolCompiler(String cobolName) {
        this.cobolName = cobolName;
    }

    public void enterCopybook(String copyName){
        copyName = IExprBaseContext.toClassName(copyName);
        if(includes.indexOf(copyName) == -1)
            includes.add(copyName);
        copybookStack.push(copyName);
        for(String copybook:copybookStack){
            List<String> copybookInclude = copybookIncludes.get(copybook);
            if(copybookInclude == null){
                copybookInclude = new ArrayList();
                copybookIncludes.put(copybook,copybookInclude);
            }
            copybookInclude.add(copyName);
        }
    }

    public String exitCopybook(String copyName){
        String ret = copybookStack.pop();
        return ret;
    }

    public List<String> getIncludesOf(String copybook) {
        return copybookIncludes.get(copybook);
    }
    public List<String> getIncludes() {
        return includes;
    }

    public String getCobolName() {
        return cobolName;
    }
    public String currentCopybook(){
        return copybookStack.peek();
    }
}
