package free.cobol2java.context;

import free.cobol2java.ICobolConvertor;
import free.cobol2java.config.CobolConfig;
import free.servpp.mustache.ILogable;

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

    default String cobol_compile(String fileName) {
        fileName = fileName.replace("\"", "");
        String fullClsName = compiledCobol.get(fileName);
        if (fullClsName != null) {
            return fullClsName;
        }
        ICobolConvertor cobolConvertor = CobolConfig.getCobolConvertor();
        List<File> files = new ArrayList<>();
        cobolConvertor.findFiles(new File(cobolConvertor.getSourcePath()), files, cobolConvertor.getSuffixes(), fileName + "*");
        if (files.size() > 0) {
            fullClsName = cobolConvertor.convertAFile(files.get(0));
        } else {
            getLogger().error("Error can not find given callsub file: {}", fileName);
            fullClsName = "UNDEFINED";
        }
        compiledCobol.put(fileName, fullClsName);
        return fullClsName;
    }
}
