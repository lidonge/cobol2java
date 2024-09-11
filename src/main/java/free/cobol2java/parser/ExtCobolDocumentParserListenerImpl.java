package free.cobol2java.parser;

import free.cobol2java.copybook.CopyBookManager;
import free.cobol2java.copybook.CopybookException;
import io.proleap.cobol.CobolPreprocessorParser;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.preprocessor.impl.CobolPreprocessorImpl;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolDocumentParserListenerImpl;
import org.antlr.v4.runtime.BufferedTokenStream;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

/**
 * @author lidong@date 2024-09-06@version 1.0
 */
public class ExtCobolDocumentParserListenerImpl extends CobolDocumentParserListenerImpl {

    public ExtCobolDocumentParserListenerImpl(CobolParserParams params, BufferedTokenStream tokens) {
        super(params, tokens);
    }
    protected String getCopyBookContent(final CobolPreprocessorParser.CopySourceContext copySource, final CobolParserParams params) {
        final File copyBook = findCopyBook(copySource, params);
        String result = null;

        if (copyBook == null) {
            result =  "";
//            LOG.warn("CobolPreprocessorException: Could not find copy book " + copySource.getText()
//                    + " in directory of COBOL input file or copy books param object.");
//			throw new CobolPreprocessorException("Could not find copy book " + copySource.getText()
//					+ " in directory of COBOL input file or copy books param object.");
        } else {
            try {
                result = new CobolPreprocessorImpl().process(copyBook, params);
                CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
                if(defaultManager.isCopybookManage()) {
                    defaultManager.loadCopyBook(copyBook,params, result);
                    result = "";
                }
            } catch (final IOException | URISyntaxException e) {
                result = null;
//                LOG.warn(e.getMessage());
            } catch (CopybookException e) {
                //not data copybook
            }
        }

        return result;
    }

}
