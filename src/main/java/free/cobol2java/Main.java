package free.cobol2java;

import free.cobol2java.copybook.CopyBookManager;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static free.cobol2java.copybook.ICobol2JavaBase.GLOBAL_FUNCTION;

/**
 * @author lidong@date 2024-08-14@version 1.0
 */
public class Main {
    public static void main(String[] args) throws IOException {
        if(args.length != 2){
            System.out.println("Usage: java -jar cobol2java.jar file_path package_name copy_dirs?");
        }
        File file = new File(args[0]);
        if(file.exists()) {
            testCost("ALL", () -> {
                List<File> copyDirs = null;
                if(args.length == 3){
                    String[] dirs = args[2].split(":");
                    copyDirs = new ArrayList<>();
                    for(String dir:dirs){
                        copyDirs.add(new File(dir));
                    }
                }
                String fileName = file.getName();
                Cobol2Java cobol2Java = new Cobol2Java(args[0],
                        fileName.substring(0, fileName.lastIndexOf(".")),copyDirs, args[1]);
                CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
                Map<String,Object> varables = new HashMap<>();
                if(defaultManager.isCopybookManage()) {
                    varables.put(GLOBAL_FUNCTION,defaultManager.getGlobalFunc());
                }
                String prog = cobol2Java.convertAll(varables);
                System.out.println(prog);
            });
        }
        else
            System.out.println("Can not find file :" + args[0]);

    }
    public static void testCost(String name, Runnable runnable){
        long curTime = System.currentTimeMillis();
        runnable.run();
        System.out.println("Execute " +name +" Cost:" + (System.currentTimeMillis() - curTime)/1000.0);
    }
}
