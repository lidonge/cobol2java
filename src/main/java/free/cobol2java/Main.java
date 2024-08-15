package free.cobol2java;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * @author lidong@date 2024-08-14@version 1.0
 */
public class Main {
    static String cblDir = "/Users/lidong/gitspace/cobol2java/src/main/COBOL/";
    static String cobolFile = cblDir + "GSA01060NC.cbl";
    public static void main(String[] args) throws IOException {
        FileInputStream fin = new FileInputStream(cobolFile);
        byte[] bytes = fin.readAllBytes();
        String s = new String(bytes,"gb2312");
        System.out.println(s);
        s=s;
    }
}
