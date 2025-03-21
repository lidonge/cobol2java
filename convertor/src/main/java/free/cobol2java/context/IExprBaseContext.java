package free.cobol2java.context;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprBaseContext extends IExprPhysicalContext{
    static String toFieldName(String cblName) {
        String[] parts = cblName.split("-");
        String ret = parts[0].toLowerCase();
        for (int i = 1; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            if(sp.matches("\\d+"))
                ret += "_" + sp;
            else {
                if(sp.length() == 0)
                    ret +="_";
                else
                    ret += IExprBaseContext.capitalizeFirstLetter(sp);
            }
        }

        return digitalStart(ret);
    }

    static String toClassName(String cblName) {
        cblName = cblName.replace("\"", "");
        String[] parts = cblName.split("-");
        String ret = "";
        for (int i = 0; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            if(sp.matches("\\d+"))
                ret += "_" + sp;
            else {
                if(sp.length() == 0)
                    ret +="_";
                else
                    ret += capitalizeFirstLetter(sp);
            }
        }
        return ret;
    }

    static boolean isBaseType(String sType){
        boolean ret = false;
        switch (sType){
            case "Integer":
            case "Double":
            case "String":
            case "Object":
                ret = true;
                break;
        }
        return ret;
    }
    private static String digitalStart(String name) {
        if (name != null && name.length() > 0 && name.charAt(0) >= '0' && name.charAt(0) <= '9')
            return "m_" + name;
        return name;
    }
    static String capitalizeFirstLetter(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
    static String lowerFirstLetter(String str) {
        return str.substring(0, 1).toLowerCase() + str.substring(1);
    }

    static void getAllTerm(ParseTree root, List<TerminalNode> list) {
        int count = root.getChildCount();
        for (int i = 0; i < count; i++) {
            ParseTree node = root.getChild(i);
            if (node instanceof TerminalNode) {
                list.add((TerminalNode) node);
            } else {
                getAllTerm(node, list);
            }
        }
    }

    default String name_toField(String cblName) {
        return toFieldName(cblName);
    }

    default String name_toClass(String cblName) {
        return toClassName(cblName);
    }

    default String name_enterClass(String fieldName) {
        return getClsLevel().push(fieldName);
    }

    default String name_exitClass() {
        return getClsLevel().pop();
    }

    default String name_curClsPath(String cur){
        String ret = "";
        for(String name:getClsLevel()){
            ret = ret + name + ".";
        }
        ret += cur;
        return ret;
    }
}
