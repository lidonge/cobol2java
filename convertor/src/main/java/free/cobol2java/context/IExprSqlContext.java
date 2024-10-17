package free.cobol2java.context;

import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.cobol2java.parser.TopCompiler;
import free.cobol2java.sql.CobolSQLCompiler;
import free.cobol2java.sql.models.SqlStatement;
import free.cobol2java.sql.handler.CobolSqlVisitor;
import free.servpp.antlr4.GeneratorErrorListener;
import free.servpp.logger.ILogable;
import free.servpp.multiexpr.MultiExprCompiler;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.IOException;
import java.net.URI;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprSqlContext extends ILogable {

    default SqlStatement sql_exec(String sql, Number line) {
        getLogger(IExprSqlContext.class).info("EXECSQL:{}",sql);
        URI file = TopCompiler.currentCompiler().getFile();
        CobolSQLCompiler exprCompiler = new CobolSQLCompiler(file, sql);
        try {
            exprCompiler.compileAntlr4(new GeneratorErrorListener(file,line.intValue()));
            ParseTree tree = exprCompiler.getTree();

            CobolSqlVisitor visitor = new CobolSqlVisitor();
            SqlStatement statement = visitor.visit(tree);
            return statement;
        } catch (IOException e) {
            getLogger(IExprSqlContext.class).error("Error Parse SQL Error:{}",e);
        }
        return null;
    }
}
