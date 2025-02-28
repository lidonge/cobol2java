package free.cobol2java.parser;

import io.proleap.cobol.CobolLexer;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.asg.exception.CobolParserException;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.asg.visitor.ParserVisitor;
import io.proleap.cobol.asg.visitor.impl.CobolCompilationUnitVisitorImpl;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * @author lidong@date 2024-09-06@version 1.0
 */
public class ExtCobolParserRunnerImpl extends CobolParserRunnerImpl {
    protected void parseCode(final String cobolCode, final String compilationUnitName, final Program program,
                             final CobolParserParams params) throws IOException {
//        LOG.info("Parsing compilation unit {}.", compilationUnitName);

        // preprocess input stream
        final String preProcessedInput = new ExtCobolPreprocessorImpl().process(cobolCode, params);

        parsePreprocessInput(preProcessedInput, compilationUnitName, program, params);
    }

    protected void parseFile(final File cobolFile, final Program program, final CobolParserParams params)
            throws IOException {
        if (!cobolFile.isFile()) {
            throw new CobolParserException("Could not find file " + cobolFile.getAbsolutePath());
        } else {
            // determine the copy book name
            final String compilationUnitName = getCompilationUnitName(cobolFile);

//            LOG.info("Parsing compilation unit {}.", compilationUnitName);

            // preprocess input stream
            final String preProcessedInput = new ExtCobolPreprocessorImpl().process(cobolFile, params);

            parsePreprocessInput(preProcessedInput, compilationUnitName, program, params);
        }
    }
    protected void parsePreprocessInput(final String preProcessedInput, final String compilationUnitName,
                                        final Program program, final CobolParserParams params) throws IOException {
        // run the lexer
        final CobolLexer lexer = new CobolLexer(CharStreams.fromString(preProcessedInput));

        if (!params.getIgnoreSyntaxErrors()) {
            // register an error listener, so that preprocessing stops on errors
            lexer.removeErrorListeners();
            lexer.addErrorListener(new ExtThrowingErrorListener());
        }

        // get a list of matched tokens
        final CommonTokenStream tokens = new CommonTokenStream(lexer);

        // pass the tokens to the parser
        final CobolParser parser = new CobolParser(tokens);

        if (!params.getIgnoreSyntaxErrors()) {
            // register an error listener, so that preprocessing stops on errors
            parser.removeErrorListeners();
            parser.addErrorListener(new ExtThrowingErrorListener());
        }

        // specify our entry point
        final CobolParser.StartRuleContext ctx = parser.startRule();

        // analyze contained compilation units
        final List<String> lines = splitLines(preProcessedInput);
        final ParserVisitor visitor = new CobolCompilationUnitVisitorImpl(compilationUnitName, lines, tokens, program);

        visitor.visit(ctx);
    }

}
