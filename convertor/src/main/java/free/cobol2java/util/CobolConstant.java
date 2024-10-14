package free.cobol2java.util;

import java.util.Arrays;

/**
 * @author lidong@date 2024-08-23@version 1.0
 */
public class CobolConstant {
    public enum Values {
        ALL ,HIGH_VALUE ,HIGH_VALUES ,LOW_VALUE ,LOW_VALUES ,NULL ,NULLS ,QUOTE ,QUOTES ,SPACE ,SPACES ,ZERO ,ZEROS ,ZEROES,SQLCODE
    }

    public static String[] texts = new String[]{"ALL" ,"HIGH-VALUE" ,"HIGH-VALUES" ,"LOW-VALUE" ,"LOW-VALUES" ,"NULL" ,"NULLS" ,"QUOTE" ,"QUOTES" ,"SPACE" ,"SPACES" ,"ZERO" ,"ZEROS" ,"ZEROES","SQLCODE"};

    public static boolean isConstant(String id){
        if(id != null)
            return Arrays.stream(texts).toList().indexOf(id) != -1;
        return false;
    }
}
