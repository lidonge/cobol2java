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
    default String expr_convertExpr(ParserRuleContext ctx) {
        return convertExpr(ctx, false);
    }

    default String expr_convertFuncParam(ParserRuleContext ctx) {
        return convertExpr(ctx, true);
    }

    default String expr_conditionReference(String fieldName) {
        String javaFieldName = name_toField(fieldName);
        String ret = "";
        String qlfName = name_qlfName(javaFieldName, null);
        String refFieldName = getJavaQlfFieldToSimpleType().get(qlfName);
        if (refFieldName == null) {
            IExprNameContext exprContext = getExprContext(javaFieldName, false);
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

    private String convertExpr(ParserRuleContext ctx, boolean isParameter) {
        List<Object> ofIds = new ArrayList<>();
        String cobolExpr = getCtxText(ctx, ofIds);
        if (isParameter) {
            cobolExpr = cobolExpr.substring(cobolExpr.indexOf("(") + 1, cobolExpr.length() - 1);
            ofIds.remove(0);
        }
        String ret = cobolExpr.replace("**", "^");
        if (ofIds.size() != 0) {
            for (Object value : ofIds) {
                ret = calcID(value, cobolExpr, ret);
            }
        } else if (CobolConstant.isConstant(ret)) {
            ret = "CobolConstant." + ret.replace("-", "_");
        }
        if (!ret.startsWith("\"") && ret.indexOf("'") != -1) {
            //FIX ALL'9'
            ret = "\"" + ret + "\"";
        }
        return ret.indexOf('^') != -1 ? ExprUtil.convertExpression(ret) : ret;

    }

    private String calcID(Object value, String cobolExpr, String ret) {
        String id = null;
        String fieldName = null;
        String ofId = null;
        boolean isLengthOf = false;
        boolean isFunction = false;
        if (value instanceof String) {
            id = (String) value;
        } else {
            IExprCtxHandler.PropOfField propOfField = ((IExprCtxHandler.PropOfField) value);
            id = propOfField.id();
//            fieldName = name_toField(propOfField.id());
        }
        String realId = id;
        if (id.startsWith(LENGTHOF)) {
            realId = id.substring(LENGTHOF.length());
            isLengthOf = true;
        }
        if (realId.startsWith(FUNCTION)) {
            realId = realId.substring(FUNCTION.length());
            isFunction = true;
        }
        fieldName = name_toField(realId);
        if (!(value instanceof String)) {
            IExprCtxHandler.PropOfField propOfField = ((IExprCtxHandler.PropOfField) value);
            ofId = propOfField.ofId().get(propOfField.ofId().size() - 1);
            id = propOfField.id();
            for (String of : propOfField.ofId()) {
                id = id + "OF" + of;
            }
        }
        if (CobolConstant.isConstant(id))
            return "CobolConstant." + id.replace("-", "_");
        String[] dims = getDimStringOfVar(cobolExpr, id);
        String dimStr = dims[1];
        String qlfName = null;
        if(isFunction){

        }else{
            qlfName = name_qlfName(fieldName, ofId);
        }
        if (dimStr == null) {
            String sExpr = qlfName;
            if(isFunction){
                sExpr = "Function." + qlfName;
            }
            if (isLengthOf) {
                sExpr = "Util.sizeof(" + qlfName + ")";
            }
            //Whole word replace
            ret = ret.replaceAll("\\b" + id + "\\b", sExpr);
        } else {
            if (dimStr.indexOf(":") == -1) {
                String qlfNameWithDims = null;
                if (getJavaQlfNameWithLeaf(qlfName) != null)
                    qlfNameWithDims = name_qlfNameWithDim(qlfName, dimStr);
                else
                    qlfNameWithDims = name_qlfUdfNameWithDim(qlfName, dimStr);
                ret = dims[0] + qlfNameWithDims + dims[2];
            } else {
                ret = dims[0] + qlfName + "(" + dimStr + ")" + dims[2];
            }
        }
        int index = ret.indexOf(":");
        if (index != -1) {
            int right = ret.indexOf(')', index);
            int left = ret.lastIndexOf('(', index);
            String range = ret.substring(left + 1, right);
            ret = "Util.subvalue(" + ret.substring(0, left) + ",\"" + range.replace(':', ',') + "\")";
        }
        return ret;
    }
}
