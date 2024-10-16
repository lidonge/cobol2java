package free.cobol2java.context;

import free.servpp.multiexpr.IEvaluatorEnvironment;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprEnvContext {
    IEvaluatorEnvironment getEnvironment();

    default void debugPoint() {
        if (getEnvironment() != null) {
            String debug = (String) getEnvironment().getVar("DEBUG");
            if ("specRegCall".equals(debug))
                debug = debug;
        }
    }
}
