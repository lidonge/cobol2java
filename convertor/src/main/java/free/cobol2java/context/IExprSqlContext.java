package free.cobol2java.context;

import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.cobol2java.sql.SqlStatement;
import free.cobol2java.sql.handler.CobolSqlVisitor;
import free.servpp.logger.ILogable;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprSqlContext extends ILogable {

    default SqlStatement sql_exec(String sql) {
        getLogger(IExprSqlContext.class).info("EXECSQL:{}",sql);
        CobolWithSqlLexer lexer = new CobolWithSqlLexer(CharStreams.fromString(sql));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CobolWithSqlParser parser = new CobolWithSqlParser(tokens);

        ParseTree tree = parser.cobolSql();

        CobolSqlVisitor visitor = new CobolSqlVisitor();
        SqlStatement statement = visitor.visit(tree);
        return statement;
    }
}
