package free.cobol2java.parser;

import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.cobol2java.copybook.CopyBookManager;
import free.cobol2java.copybook.CopybookException;
import free.cobol2java.sql.SqlInclude;
import free.cobol2java.sql.handler.CobolSqlVisitor;
import free.servpp.logger.ILogable;
import io.proleap.cobol.CobolPreprocessorParser;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.exception.CobolPreprocessorException;
import io.proleap.cobol.preprocessor.sub.CobolLine;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolDocumentParserListenerImpl;
import io.proleap.cobol.preprocessor.sub.util.TokenUtils;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

/**
 * @author lidong@date 2024-09-06@version 1.0
 */
public class ExtCobolDocumentParserListenerImpl extends CobolDocumentParserListenerImpl implements ILogable {
    private final CobolParserParams params;

    private final BufferedTokenStream tokens;

    public ExtCobolDocumentParserListenerImpl(CobolParserParams params, BufferedTokenStream tokens) {
        super(params, tokens);
        this.params = params;
        this.tokens = tokens;
    }

    @Override
    public void exitExecSqlStatement(final CobolPreprocessorParser.ExecSqlStatementContext ctx) {
        // throw away EXEC SQL terminals
        pop();

        // a new context for the SQL statement
        push();

        /*
         * text
         */
        final String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
        if (text.indexOf("INCLUDE") != -1) {
            SqlInclude statement = getSqlInclude(text);

            final String copyBookContent = getCopyBookContent(statement);

            if (copyBookContent != null) {
                context().write(copyBookContent + CobolPreprocessor.NEWLINE);
                context().replaceReplaceablesByReplacements(tokens);
            }

        } else {
            final String linePrefix = CobolLine.createBlankSequenceArea(params.getFormat())
                    + CobolPreprocessor.EXEC_SQL_TAG;
            final String lines = buildLines(text, linePrefix);

            context().write(lines);
        }
        final String content = context().read();
        pop();

        context().write(content);
    }

    private static SqlInclude getSqlInclude(String text) {
        SqlInclude statement;
        CobolWithSqlLexer lexer = new CobolWithSqlLexer(CharStreams.fromString(text));
        CommonTokenStream tokensStream = new CommonTokenStream(lexer);
        CobolWithSqlParser parser = new CobolWithSqlParser(tokensStream);

        ParseTree tree = parser.cobolSql();

        CobolSqlVisitor visitor = new CobolSqlVisitor();
        statement = (SqlInclude) visitor.visit(tree);
        return statement;
    }

    protected String getCopyBookContent(final SqlInclude copySource) {
        final File copyBook = findCopyBook(copySource);
        return getCopyBookContent("", copyBook, params, true);
    }

    protected File findCopyBook(final SqlInclude copySource) {
        final File result;

        if (copySource.getIncludeId() != null) {
            result = new SqlCobolWordCopyBookFinderImpl().findCopyBook(params, copySource.getIncludeId());
        } else if (copySource.getIncludeFile() != null) {
            result = new SqlFilenameCopyBookFinderImpl().findCopyBook(params, copySource.getIncludeFile());
        } else {
            result = null;
        }

        return result;
    }

    @Override
    protected String getCopyBookContent(final CobolPreprocessorParser.CopySourceContext copySource, final CobolParserParams params) {
        final File copyBook = findCopyBook(copySource, params);
        return getCopyBookContent("             75 " + copySource.getText() + ".", copyBook, params, true);
    }

    private String getCopyBookContent(final String copySource, final File copyBook, final CobolParserParams params, boolean normalCopyBook) {
        String result = null;

        if (copyBook == null) {
            result = copySource;
//            LOG.warn("CobolPreprocessorException: Could not find copy book " + copySource.getText()
//                    + " in directory of COBOL input file or copy books param object.");
//			throw new CobolPreprocessorException("Could not find copy book " + copySource.getText()
//					+ " in directory of COBOL input file or copy books param object.");
        } else {
            String copyName = copyBook.getName();
            CobolCompiler cobolCompiler = TopCompiler.currentCompiler();
            cobolCompiler.enterCopybook(copyName);
            try {
                File parent = copyBook.getParentFile();
                List<File> copyBookDirectories = params.getCopyBookDirectories();
                params.setCopyBookDirectories(copyBookDirectories);
                result = new ExtCobolPreprocessorImpl().process(copyBook, params);
                CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
                if (normalCopyBook && defaultManager.isCopybookManage()) {
                    defaultManager.loadCopyBook(copyBook, params, result);
                    if(!copySource.endsWith("CONST."))
                        result = copySource;
                    else
                        result = "";
                }
            } catch (final IOException | URISyntaxException e) {
                result = null;
//                LOG.warn(e.getMessage());
            } catch (CopybookException e) {
                //not data copybook
            }catch(CobolPreprocessorException ex){
                getLogger(getClass()).error("Error when parse COPYBOOK:{} with exception {}", copyBook.getName(),ex.getMessage());
                result = null;
            }finally {
                cobolCompiler.exitCopybook(copyName);
            }
        }

        return result;
    }

    @Override
    public void exitCopyStatement(final CobolPreprocessorParser.CopyStatementContext ctx) {
        pop();
        String[] lines = context().read().split("\n");
        boolean normalCopyBook = true;
        boolean sameNameCopy = false;
        for (int i = lines.length - 1; i > 0; i--) {
            String line = lines[i].trim();

            if (line.isEmpty())
                continue;
            if(line.startsWith("*"))
                continue;
            if (line.indexOf("PIC") != -1 || line.startsWith("LINKAGE") ||
                    line.startsWith("WORKING") || line.startsWith("COPY") || line.startsWith("75")) {
                normalCopyBook = false;
            }else{
                String copyText = ctx.copySource().getText();
                line = line.replace('\t',' ');

                if(line.substring(line.lastIndexOf(' ')+1).equals(copyText+".")){
                    sameNameCopy = true;
                }
            }
            break;
        }
        push();

        /*
         * replacement phrase
         */
        for (final CobolPreprocessorParser.ReplacingPhraseContext replacingPhrase : ctx.replacingPhrase()) {
            context().storeReplaceablesAndReplacements(replacingPhrase.replaceClause());
        }

        /*
         * copy the copy book
         */
        final CobolPreprocessorParser.CopySourceContext copySource = ctx.copySource();
        String copyBookContent = null;
        if(normalCopyBook){
            if(sameNameCopy){
                copyBookContent = getCopyBookContent("", findCopyBook(copySource, params), params, true);
            }else {
                copyBookContent = getCopyBookContent(copySource, params);
            }
        }else {
            copyBookContent = getCopyBookContent("", findCopyBook(copySource, params), params, false);
        }

        if (copyBookContent != null) {
            context().write(copyBookContent + CobolPreprocessor.NEWLINE);
            context().replaceReplaceablesByReplacements(tokens);
        }

        final String content = context().read();
        pop();

        context().write(content);
    }
}
