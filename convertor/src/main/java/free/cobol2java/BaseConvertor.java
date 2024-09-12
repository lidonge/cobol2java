package free.cobol2java;

import com.typesafe.config.Config;
import free.servpp.config.IConfig;
import free.servpp.config.hocon.HoconConfigTypeManager;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong@date 2024-09-11@version 1.0
 */
public class BaseConvertor {
    String sourcePath;
    String targetPath;
    List<File> copyDirs;
    String rootPackageName;
    String format;
    String encoding;
    boolean copybookManage = false;
    protected void initConfig(String[] args) {
        sourcePath = args[0];
        targetPath = args[1];
        String[] dirs = args[2].split(":");
        copyDirs = new ArrayList<>();
        for(String dir:dirs){
            File fDir = new File(dir);
            if(!fDir.exists()){
                fDir = new File(sourcePath,dir);
            }

            copyDirs.add(fDir);
        }
        rootPackageName = args[3];
        format = args[4];
        encoding = args.length == 6 ? args[5] : "utf-8";

    }


    protected void initConfig(HoconConfigTypeManager manager) {
        IConfig config = manager.getHoconConfigManager("application").getConfigById("cobol2java");
        Config con = (Config) config.getConfigObject();
        sourcePath = con.getString("application.dirs.sourcePath");
        targetPath = con.getString("application.dirs.targetPath");
        String[] dirs = con.getString("application.dirs.copyDir").split(":");
        copyDirs = new ArrayList<>();
        for(String dir:dirs){
            File fDir = new File(dir);
            if(!fDir.exists()){
                fDir = new File(sourcePath,dir);
            }

            copyDirs.add(fDir);
        }
        rootPackageName = con.getString("application.rootPackageName");
        format = con.getString("application.format");
        encoding = con.getString("application.encoding");
        copybookManage = con.getBoolean("application.copybookManage");
    }
}
