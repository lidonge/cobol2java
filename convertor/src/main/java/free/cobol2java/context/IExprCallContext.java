package free.cobol2java.context;

import free.cobol2java.ICobolConvertor;
import free.cobol2java.config.CobolConfig;
import free.servpp.logger.ILogable;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprCallContext extends ILogable {
    /**
     * Global map of cobol sub-program stored
     * Key is cobol shorten file name
     * Value is full class name
     */
    static Map<String, String> compiledCobol = new HashMap<>();
    static Map<String, String> unCompiledCobol = new HashMap<>();

    default String cobol_compile(String fileName) {
        fileName = fileName.replace("\"", "");
        return compile(fileName);
    }

    static String getFullClassNameOfCobolFile(String fileName){
        return compiledCobol.get(fileName);
    }
    static void saveFullClassNameOfCobolFile(String fileName, String fullClsName){
        compiledCobol.put(fileName, fullClsName);
    }
    private static String compile(String fileName) {
        String fullClsName = getFullClassNameOfCobolFile(fileName);
        if (fullClsName != null) {
            return fullClsName;
        }
        ICobolConvertor cobolConvertor = CobolConfig.getCobolConvertor();
        List<File> files = new ArrayList<>();
        cobolConvertor.findFiles(new File(cobolConvertor.getSourcePath()), files,
                cobolConvertor.getSuffixes(), fileName + ".*");
        if (files.size() > 0) {
            if(files.size() > 1)
                LoggerFactory.getLogger(IExprCallContext.class).warn("Compiling sub duplicate cbl {}:{}",fileName, files);
            else
                LoggerFactory.getLogger(IExprCallContext.class).info("Compiling sub cbl {}:{}",fileName, files.get(0));
            fullClsName = cobolConvertor.convertAFile(files.get(0));
        } else {
//            getLogger().error("Error can not find given callsub file: {}", fileName);
            fullClsName = "UNDEFINED";
        }
        saveFullClassNameOfCobolFile(fileName, fullClsName);
        return fullClsName;
    }

    default String cobol_precompile(String fileName){
        fileName = fileName.replace("\"", "");
        String fullClsName = unCompiledCobol.get(fileName);
        if (fullClsName != null) {
            return fullClsName;
        }
        ICobolConvertor cobolConvertor = CobolConfig.getCobolConvertor();
        List<File> files = new ArrayList<>();
        cobolConvertor.findFiles(new File(cobolConvertor.getSourcePath()), files,
                cobolConvertor.getSuffixes(), fileName + "*");
        if (files.size() > 0) {
            fullClsName = getFullClassName(files.get(0));
        } else {
            getLogger().error("Error can not find given callsub file: {}", fileName);
            fullClsName = "UNDEFINED";
        }
        unCompiledCobol.put(fileName, fullClsName);
        return fullClsName;
    }
    static void compileAllSub(){
        do {
            List<String> keyList = new ArrayList<>();
            for (String key : unCompiledCobol.keySet()) {
                if (compiledCobol.get(key) == null) {
                    keyList.add(key);
                }
            }
            if (keyList.size() != 0) {
                for (String key : keyList) {
                    compile(key);
                }
            }else{
                break;
            }
        }while(true);
    }
    private static String getFullClassName(File file) {
        ICobolConvertor cobolConvertor = CobolConfig.getCobolConvertor();
        String relativePath = file.getAbsolutePath().substring(cobolConvertor.getSourcePath().length() + 1);
        String fileName = file.getName();
        String className = fileName.substring(0, fileName.lastIndexOf("."));
        className = className.substring(0, 1).toUpperCase() + className.substring(1).toLowerCase();
        String relativeParent = new File(relativePath).getParent();
        String packageName = cobolConvertor.getRootPackageName() +
                (relativeParent != null ? "." + relativeParent.replace(File.separator, ".") : "");

        return ICobolConvertor.checkPackageName(packageName+"."+className);
    }
}
