package free.cobol2java.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class Func {
    private Map<String,String> fieldToQualifiedName = new HashMap<>();
    private Stack<String> clsLevel = new Stack<>();
    public String name_toClass(String cblName){
        String[] parts = cblName.split("-");
        String ret = "";
        for(int i = 0;i<parts.length;i++){
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }
        return ret;
    }
    public String name_toField(String cblName){
        String[] parts = cblName.split("-");
        String ret = parts[0].toLowerCase();
        for(int i = 1;i<parts.length;i++){
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }

        return ret;
    }

    public String name_enterClass(String fieldName){
        return clsLevel.push(fieldName);
    }
    public String name_exitClass(){
        return clsLevel.pop();
    }
    public String name_putInnerField(String fieldName){
        String qualifiedName = clsLevel.size() == 0 ? null : createQualifedName(fieldName);
        if(qualifiedName != null)
            fieldToQualifiedName.put(fieldName,qualifiedName);
        return qualifiedName;
    }

    public String name_delegateName(String fieldName){
        String ret = fieldToQualifiedName.get(fieldName);
        if(ret == null)
            ret = fieldName;
        return ret;
    }
    private String createQualifedName(String fieldName) {
        String ret = "";
        for(String superField:clsLevel){
            ret +=superField+".";
        }
        return ret + fieldName;
    }

    public String getQualifedName(String fieldName){
        return fieldToQualifiedName.get(fieldName);
    }

    private static String capitalizeFirstLetter(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
