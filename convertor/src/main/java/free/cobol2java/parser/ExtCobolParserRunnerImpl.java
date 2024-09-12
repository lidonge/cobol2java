package free.cobol2java.parser;

import io.proleap.cobol.asg.exception.CobolParserException;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.impl.CobolPreprocessorImpl;

import java.io.File;
import java.io.IOException;

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
}
