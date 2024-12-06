package free.cobol2java.context;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigObject;

import java.util.HashMap;
import java.util.Map;

/**
 * @author lidong@date 2024-12-06@version 1.0
 */
public class TypeReplacement {
    private static class FromTo{
        public String from;
        public String to;

        public FromTo(String from, String to) {
            this.from = from;
            this.to = to;
        }
    }
    private static Map<String, FromTo> fromTos = new HashMap<>();

    private static void put(String field, String from, String to){
        fromTos.put(field,new FromTo(from, to));
    }

    public static String replace(String field, String orgType){
        String ret = null;
        FromTo fromTo = fromTos.get(field);
        if(fromTo != null && fromTo.from.equals(orgType)){
            ret = fromTo.to;
        }
        return ret;
    }

    public static void init(Config con){
        ConfigObject configObject = con.getObject("application.typeReplacements");
        for(Map.Entry entry :configObject.entrySet()){
            String field = (String) entry.getKey();
            ConfigObject fromTo = (ConfigObject) entry.getValue();
            put(field,fromTo.get("from").unwrapped().toString(), fromTo.get("to").unwrapped().toString());
        }
    }
}
