package free.cobol2java.config;

import free.cobol2java.ICobolConvertor;

/**
 * @author lidong@date 2024-09-13@version 1.0
 */
public class CobolConfig {
    private static ICobolConvertor cobolConvertor;

    public synchronized static ICobolConvertor getCobolConvertor() {
        return cobolConvertor;
    }

    public synchronized static void setCobolConvertor(ICobolConvertor cobolConvertor) {
        CobolConfig.cobolConvertor = cobolConvertor;
    }
}
