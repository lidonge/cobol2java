package free.cobol2java;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

/**
 * @author lidong@date 2023-11-23@version 1.0
 */
public class CodeFormator {

    public static String formatCode(String code) {
        BufferedReader reader = new BufferedReader(new StringReader(code));
        StringBuffer sb = new StringBuffer();
        String line;
        try {
            while ((line = reader.readLine()) != null) {
                if (line.trim().length() == 0) {

                } else {
                    sb.append(line).append("\n");
                }
            }
        }catch (IOException e){
            e.printStackTrace();
        }
        return sb.toString();
    }

}
