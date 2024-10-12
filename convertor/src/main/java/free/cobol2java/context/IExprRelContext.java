package free.cobol2java.context;

import free.cobol2java.util.RelationalOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprRelContext {

    default String rel_getOper(String cobolOper, String left, String right) {
        String ret = null;
        if(left.indexOf("SBCA-CNTWK-REL-CNT") != -1){
            left = left;
        }
        RelationalOperator oper = RelationalOperator.valueOf(cobolOper);
//        List<String> lefts = extractQualifiedNames(left);
//        List<String> rights = extractQualifiedNames(right);
        switch (oper) {
            case EQUAL:
            case EQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") == 0";
                break;
            case LESS:
            case LESSTHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") < 0";
                break;
            case GREATER:
            case MORETHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") > 0";
                break;
            case NOT_EQUAL:
            case NOTEQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") != 0";
                break;
            case LESS_OR_EQUAL:
            case LESSTHANOREQUAL:
                ret = "Util.compare(" + left + "," + right + ") <= 0";
                break;
            case GREATER_OR_EQUAL:
            case MORETHANOREQUAL:
                ret = "Util.compare(" + left + "," + right + ") >= 0";
                break;
        }
        return ret;
    }

    private List<String> extractQualifiedNames(String expression) {
        // 使用 replaceAll 方法替换匹配的数组部分为空字符串
        expression = expression.replaceAll("\\[[0-9]*\\]", "");
        // 定义匹配 qualifiedName 的正则表达式
        String regex = "\\b[a-zA-Z_][a-zA-Z0-9_]*(\\.[a-zA-Z_][a-zA-Z0-9_]*)*\\b";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(expression);

        // 存储所有匹配到的 qualifiedName 变量
        List<String> qualifiedNames = new ArrayList<>();
        while (matcher.find()) {
            qualifiedNames.add(matcher.group());
        }
        return qualifiedNames;
    }

}
