package free.cobol2java.context;

import java.util.List;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprCommentContext {

    default String cbl_getComment(int start, String ctxLine, List<String> lines) {
        String ret = null;
        for (int i = start - 2; i >= 0; i--) {
            String line = lines.get(i);
            String trim = line.trim();
            if (trim.startsWith("*>") && !trim.startsWith("*>EXECSQL")) {
                if (ret == null)
                    ret = "//" + trim.substring(2);
                else
                    ret += "\n//" + trim.substring(2);
            } else if (trim.length() != 0)
                break;
        }
        return ret;
    }
}
