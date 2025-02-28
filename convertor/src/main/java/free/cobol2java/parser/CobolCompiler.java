package free.cobol2java.parser;

import free.cobol2java.context.IExprBaseContext;

import java.net.URI;
import java.util.*;

/**
 * @author lidong@date 2024-09-30@version 1.0
 */
public class CobolCompiler {
    private String cobolName;
    private URI file;
    private List<String> includes = new ArrayList<>();
    private Stack<String> copybookStack = new Stack<>();
    private Map<String, List<String>> copybookIncludes = new HashMap<>();

    private List<Integer> beginCopes = new ArrayList<>();
    private List<Integer> cpyLengths = new ArrayList<>();


    public CobolCompiler(String cobolName, URI file) {
        this.cobolName = cobolName;
        this.file = file;
    }

    private void createCopybookIncludes(String copybook) {
        if (copybookIncludes.get(copybook) == null) {
            List<String> copybookInclude = new ArrayList();
            copybookIncludes.put(copybook, copybookInclude);
        }
    }

    public void enterCopybook(String copyName) {
        copyName = IExprBaseContext.toClassName(copyName);
        createCopybookIncludes(copyName);
        List<String> copybookInclude = includes;
        if (copybookStack.size() != 0) {
            String copybook = copybookStack.peek();
            copybookInclude = copybookIncludes.get(copybook);
        }
        if (copybookInclude.indexOf(copyName) == -1)
            copybookInclude.add(copyName);
        copybookStack.push(copyName);
    }

    public String exitCopybook(String copyName) {
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

    public String currentCopybook() {
        return copybookStack.peek();
    }

    public URI getFile() {
        return file;
    }

    public void addCopybookLines(int beginCope, int cpyLength) {
        beginCopes.add(beginCope);
        cpyLengths.add(cpyLength);
    }

    public List<Integer> getBeginCopes() {
        return beginCopes;
    }

    public List<Integer> getCpyLengths() {
        return cpyLengths;
    }

    public int caclRealLine(int line) {
        int realLine = line;
        int count = 0;
        for (int i = 0; i < beginCopes.size(); i++) {
            int beginCope = beginCopes.get(i);
            int cpyLength = cpyLengths.get(i);
            if (line > beginCope + cpyLength) {
                realLine -=  cpyLength;
            } else {
                count = i;
                break;
            }
        }
        return realLine + count;
    }
}
