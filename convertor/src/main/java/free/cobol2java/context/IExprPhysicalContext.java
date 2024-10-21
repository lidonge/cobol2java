package free.cobol2java.context;

import java.util.Map;
import java.util.Stack;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprPhysicalContext {
    String getCopyBookName();
    String getCopyBookPath();

    Stack<String> getClsLevel();
    Map<String, String> getJavaQlfFieldToFullType();
    Map<String, String> getJavaFieldToQualifiedName();
    Map<String, String> getJavaFieldToQlfNameWithLeaf();
    Map<String, String> getJavaQlfFieldToSimpleType();

    Map<String, Number> getJavaFieldNameToDim();
    Map<String, String> getInnerClsNameToCopybookName();
    Map<String, ExprContext> getCopybookContexts();
    Map<String, String> getQlfNameToCopyFieldName();
    Map<String, String> getCopyFieldNameToQlfName();
}
