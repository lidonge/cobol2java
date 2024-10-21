package free.cobol2java.context;

import free.cobol2java.copybook.CopyBookManager;
import free.servpp.logger.ILogable;

import java.util.*;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class ExprContext extends ExprBaseContext implements ILogable,
        IExprBaseContext, IExprDimensionContext, IExprInitContext, IExprValueContext,
        IExprPhysicalContext,IExprRelContext,IExprSqlContext,IExprSetContext,
        IExprCommentContext,IExprCallContext,ICopybookContext,IExprCtxHandler, IExprConvertContext {
    private String copyBookName;
    private String copyBookPath;
    private ExprContext orMappingContext;
    //    private ExprContext copybookContext;

    private Map<String, ExprContext> copybookContexts = CopyBookManager.getDefaultManager().getGlobalFunc();
    /**
     * Set when field defined(PIC or 01 FIELD-NAME.)
     * Or if the field is a condition reference, type will be the enum value name of the erference.
     */
    private Map<String, String> javaQlfFieldToSimpleType = new HashMap<>();
    /**
     * If the field is not a base type, store the full class name.
     */
    private Map<String, String> javaQlfFieldToFullType = new HashMap<>();
    /**
     * Put when field defined(PIC A(10)..., or 01 FIELD-NAME. COPY COPYBOOK.)
     * If field level is 75, use super field name
     * There maybe ambiguous, a field name match more-then one qlfName, so use '|' to separate them.
     */
    private Map<String, String> javaFieldToQualifiedName = new HashMap<>();
    /**
     * Put when qlf_name is creating, each level name all point to qlf_name
     * Maybe ambiguous as #javaFieldToQualifiedName
     */
    private Map<String, String> javaFieldToQlfNameWithLeaf = new HashMap<>();
    /**
     * Set when field defined(PIC A(10)..., or 01 FIELD-NAME. COPY COPYBOOK.)
     */
    private Map<String, Number> javaFieldNameToDim = new HashMap<>();
    /**
     * If field level is 75,means inner class name should be the copybook name.
     * Set when copy75, the key is qlfName in the copybook, and value is fieldName in main cbl.
     * There maybe ambiguous, a field name match more-then one qlfName, so use '|' to separate them.
     */
    private Map<String, String> copyFieldNameToQlfName = new HashMap<>();
    /**
     * If a field is a copybook, the map store the qlfName of the field to the copyFieldName.
     */
    private Map<String, String> qlfNameToCopyFieldName = new HashMap<>();
    /**
     * If field level is 75,means inner class name should be the copybook name.
     * Set when copy75, the key is innerClassName in main cbl, and value is copybook class name.
     */
    private Map<String, String> innerClsNameToCopybookName = new HashMap<>();
    /**
     * The field of nested class.
     * class A{
     * class B{
     * class C{
     * class D{
     * <p>
     * }
     * D d;
     * }
     * C c;
     * }
     * B b;
     * }
     * The stack should be [c,b]
     */
    private Stack<String> clsLevel = new Stack<>();

    @Override
    public String getCopyBookName() {
        return copyBookName;
    }

    public void setCopyBookName(String copyBookName) {
        this.copyBookName = copyBookName;
    }

    @Override
    public String getCopyBookPath() {
        return copyBookPath;
    }

    public void setCopyBookPath(String copyBookPath) {
        this.copyBookPath = copyBookPath;
    }

    public ExprContext getOrMappingContext() {
        return orMappingContext;
    }

    public void setOrMappingContext(ExprContext orMappingContext) {
        this.orMappingContext = orMappingContext;
    }

    @Override
    public Map<String, String> getCopyFieldNameToQlfName() {
        return copyFieldNameToQlfName;
    }

    @Override
    public Map<String, String> getQlfNameToCopyFieldName() {
        return qlfNameToCopyFieldName;
    }

    @Override
    public Map<String, String> getJavaQlfFieldToFullType() {
        return javaQlfFieldToFullType;
    }

    @Override
    public Map<String, ExprContext> getCopybookContexts() {
        return copybookContexts;
    }

    @Override
    public Map<String, String> getInnerClsNameToCopybookName() {
        return innerClsNameToCopybookName;
    }

    @Override
    public Map<String, Number> getJavaFieldNameToDim() {
        return javaFieldNameToDim;
    }

    @Override
    public Map<String, String> getJavaQlfFieldToSimpleType() {
        return javaQlfFieldToSimpleType;
    }

    @Override
    public Map<String, String> getJavaFieldToQualifiedName() {
        return javaFieldToQualifiedName;
    }

    @Override
    public Map<String, String> getJavaFieldToQlfNameWithLeaf() {
        return javaFieldToQlfNameWithLeaf;
    }

    @Override
    public Stack<String> getClsLevel() {
        return clsLevel;
    }

}
