package free.cobol2java.context;

import free.cobol2java.java.RelationalOperator;

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
}
