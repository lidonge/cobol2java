package free.cobol2java.sql;

import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.servpp.antlr4.AntlrCompiler;
import org.antlr.v4.runtime.*;

import java.io.IOException;
import java.net.URI;

/**
 * @author lidong@date 2024-10-17@version 1.0
 */
public class CobolSQLCompiler extends AntlrCompiler {
    private String sql;
    public CobolSQLCompiler(URI antlrUri,String sql) {
        super(antlrUri);
        this.sql = sql;
    }

    @Override
    protected String getAntlrText() throws IOException {
        return sql;
    }

    @Override
    public Lexer createLexer(CharStream stream) {
        return new CobolWithSqlLexer(stream);
    }

    @Override
    public Parser createParser(CommonTokenStream tokens) {
        return new CobolWithSqlParser(tokens);
    }

    @Override
    public ParserRuleContext executeParser(Parser parser) {
        return ((CobolWithSqlParser)parser).cobolSql();
    }
}
