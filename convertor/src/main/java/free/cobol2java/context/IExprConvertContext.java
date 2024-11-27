package free.cobol2java.context;

import free.cobol2java.util.CobolConstant;
import free.cobol2java.util.ExprUtil;
import org.antlr.v4.runtime.ParserRuleContext;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprConvertContext extends IExprNameContext, IExprRelContext {
    default String expr_conditionReference(String fieldName) {
        String javaFieldName = name_toField(fieldName);
        String ret = "";
        String qlfName = name_qlfName(javaFieldName, null);
        if(qlfName.startsWith("UNDEFINED_")){
            getLogger().error("Error undefined field {}.", fieldName);
            return qlfName;
        }
        String refFieldName = getJavaQlfFieldToSimpleType().get(qlfName);
        if (refFieldName == null) {
            IExprNameContext exprContext = getExprContext(javaFieldName);
            refFieldName = exprContext.getJavaQlfFieldToSimpleType().get(qlfName);
        }

        if (refFieldName != null && !IExprBaseContext.isBaseType(refFieldName)) {
            ret = "java.util.Arrays.asList(" + qlfName + ").contains(" + name_qlfName(refFieldName, null) + ")";
            //ret = name_qlfName(refFieldName, null) + "==" + qlfName;
        } else {
            String isEnum = ""+getEnvironment().getVar("isEnum");
            if(isEnum.equals("isEnum")) {
                getLogger().error("Error UNKNOW_CONDITION_REF: " + fieldName);
                ret = "UNKNOW_CONDITION_REF" + "==" + qlfName;
            }else {
                String left = "" + getEnvironment().getVar("left");
                String relationalOperatorType = "" + getEnvironment().getVar("relationalOperatorType");
                ret = rel_getOper(relationalOperatorType,left,qlfName);
            }
        }
        return ret;
    }

}
