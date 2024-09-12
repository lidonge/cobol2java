package free.cobol2java.parser;

import io.proleap.cobol.preprocessor.impl.CobolPreprocessorImpl;
import io.proleap.cobol.preprocessor.sub.document.CobolDocumentParser;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolDocumentParserImpl;

/**
 * @author lidong@date 2024-09-06@version 1.0
 */
public class ExtCobolPreprocessorImpl extends CobolPreprocessorImpl {
    protected CobolDocumentParser createDocumentParser() {
        return new ExtCobolDocumentParserImpl();
    }
}
