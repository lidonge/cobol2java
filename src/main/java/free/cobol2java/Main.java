package free.cobol2java;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * @author lidong@date 2024-08-14@version 1.0
 */
public class Main {
    public static void main(String[] args) throws IOException {
        if(args.length != 2){
            System.out.println("Usage: java -jar cobol2java.jar file_path package_name");
        }
        File file = new File(args[0]);
        if(file.exists())
            testCost("ALL",() -> {
                String fileName = file.getName();
                Cobol2Java cobol2Java = new Cobol2Java(args[0], fileName.substring(0, fileName.lastIndexOf(".")),args[1]);
                String prog = cobol2Java.convertAll();
                System.out.println(prog);
            });
        else
            System.out.println("Can not find file :" + args[0]);

    }
    public static void testCost(String name, Runnable runnable){
        long curTime = System.currentTimeMillis();
        runnable.run();
        System.out.println("Execute " +name +" Cost:" + (System.currentTimeMillis() - curTime)/1000.0);
    }
}
