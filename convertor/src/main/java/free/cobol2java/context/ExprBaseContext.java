package free.cobol2java.context;

import free.servpp.multiexpr.IEvaluatorEnvironment;

import java.util.Stack;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public class ExprBaseContext implements IExprEnvContext{
    private IEvaluatorEnvironment environment;
    private Stack<Integer> curDims = new Stack<>();
    private Stack<Object> variables = new Stack<>();

    public IEvaluatorEnvironment getEnvironment() {
        return environment;
    }

    public void setEnvironment(IEvaluatorEnvironment environment) {
        this.environment = environment;
    }

    public Object var_push(Object var) {
        return variables.push(var);
    }

    public Object var_pop() {
        return variables.pop();
    }

    public Object var_peek() {
        return variables.peek();
    }

    public int dim_push(Number dim) {
        return curDims.push(dim.intValue());
    }

    public int dim_pop() {
        return curDims.pop();
    }

    public int dim_peek() {
        return curDims.peek();
    }

    public int dim_size() {
        return curDims.size();
    }

    public String dim_value() {
        String ret = "";
        for (int i = 0; i < curDims.size(); i++) {
            if (i != 0)
                ret += ",";
            ret += curDims.get(i);
        }
        return ret;
    }

    public String dim_udfCall(String ctxText) {
        String ret = null;
        int idx = ctxText.indexOf('(');
        if (idx != -1) {
            ret = ctxText.substring(idx + 1, ctxText.indexOf(')'));
        }
        return ret;
    }
}
