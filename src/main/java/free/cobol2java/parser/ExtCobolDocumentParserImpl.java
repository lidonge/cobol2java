package free.cobol2java.parser;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.preprocessor.sub.document.CobolDocumentParserListener;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolDocumentParserImpl;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolDocumentParserListenerImpl;
import org.antlr.v4.runtime.CommonTokenStream;

/**
 * @author lidong@date 2024-09-06@version 1.0
 */
public class ExtCobolDocumentParserImpl extends CobolDocumentParserImpl {
    protected CobolDocumentParserListener createDocumentParserListener(final CobolParserParams params,
                                                                       final CommonTokenStream tokens) {
        return new ExtCobolDocumentParserListenerImpl(params, tokens);
    }

}
