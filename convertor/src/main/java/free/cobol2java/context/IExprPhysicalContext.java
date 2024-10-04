package free.cobol2java.context;

import java.util.Map;
import java.util.Stack;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprPhysicalContext {
    String getCopyBookName();
    Stack<String> getClsLevel();
    Map<String, String> getFieldToClassType();
    Map<String, String> getJavaFieldToQualifiedName();
    Map<String, String> getJavaFieldToQlfNameWithLeaf();
    Map<String, String> getJavaQlfFieldToType();

    Map<String, Number> getJavaFieldNameToDim();
    Map<String, String> getInnerClsNameToCopybookName();
    Map<String, ExprContext> getCopybookContexts();
    Map<String, String> getJavaFieldNameToCopyFieldName();
    Map<String, String> getCopyFieldNameToJavaFileName();
}
