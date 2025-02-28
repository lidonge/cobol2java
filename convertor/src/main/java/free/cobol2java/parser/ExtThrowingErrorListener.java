package free.cobol2java.parser;

import io.proleap.cobol.asg.runner.ThrowingErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

/**
 * @author lidong@date 2025-02-28@version 1.0
 */
public class ExtThrowingErrorListener extends ThrowingErrorListener {
    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
        int realLine = TopCompiler.currentCompiler().caclRealLine(line);
        super.syntaxError(recognizer, offendingSymbol, realLine, charPositionInLine, msg, e);
    }
}
