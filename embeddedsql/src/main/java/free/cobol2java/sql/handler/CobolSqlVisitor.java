package free.cobol2java.sql.handler;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */

import free.cobol2java.antlr.CobolWithSqlBaseVisitor;
import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.cobol2java.sql.*;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class CobolSqlVisitor extends CobolWithSqlBaseVisitor<SqlStatement> {
    @Override
    public SqlStatement visitDeclareCursor(CobolWithSqlParser.DeclareCursorContext ctx) {
        String cursorName = ctx.cursorName().getText();

        // Visit the select query
        SelectQuery selectQuery = (SelectQuery) visit(ctx.selectQuery());

        // Check if FOR UPDATE clause is present
        boolean forUpdate = ctx.forUpdateClause() != null;

        return new CursorDeclaration(cursorName, selectQuery, forUpdate);
    }

    @Override
    public SqlStatement visitSqlInclude(CobolWithSqlParser.SqlIncludeContext ctx) {
        SqlInclude ret = new SqlInclude();
        if (ctx.identifier() != null) {
            String includeId = ctx.identifier().getText();
            ret.setIncludeId(includeId);
        } else if (ctx.FILE_LITERAL() != null) {
            String includeFile = ctx.FILE_LITERAL().getText().replace("'", "");
            ret.setIncludeFile(includeFile);
        }
        return ret;
    }

    @Override
    public SqlStatement visitSqlStatement(CobolWithSqlParser.SqlStatementContext ctx) {
        SqlStatement ret = null;
        CobolWithSqlParser.SqlQueryContext sqlQuery = ctx.sqlQuery();
        if(sqlQuery != null)
            ret = super.visitSqlQuery(sqlQuery);
        if(ctx.sqlCursorOperation() != null)
            ret = visitSqlCursorOperation(ctx.sqlCursorOperation());

        if(ctx.errorHandle() != null){
            ret = visitErrorHandle(ctx.errorHandle());
        }
        return ret;
    }

    @Override
    public SqlStatement visitErrorHandle(CobolWithSqlParser.ErrorHandleContext ctx) {
        return super.visitErrorHandle(ctx);
    }

    @Override
    public SqlStatement visitSqlCursorOperation(CobolWithSqlParser.SqlCursorOperationContext ctx) {
        //TODO
        return super.visitSqlCursorOperation(ctx);
    }

    @Override
    public SqlStatement visitSelectQuery(CobolWithSqlParser.SelectQueryContext ctx) {
        SelectQuery selectQuery = new SelectQuery();

        // Extract columns
        CobolWithSqlParser.ColumnListContext columnListContext = ctx.columnList();
        if(columnListContext != null) {
            List<String> columns = new ArrayList<>();

            for (CobolWithSqlParser.ColumnNameContext columnCtx : columnListContext.columnName()) {
                columns.add(columnCtx.getText());
            }
            selectQuery.setColumns(columns);
        }else{
            if(ctx.allColumn() != null)
                selectQuery.setAllColumn(true);
        }

        // Extract COBOL variables
        CobolWithSqlParser.CobolVariableListContext cobolVariableListContext = ctx.cobolVariableList();
        if(cobolVariableListContext != null) {
            List<String> cobolVars = new ArrayList<>();
            for (CobolWithSqlParser.CobolVariableContext varCtx : cobolVariableListContext.cobolVariable()) {
                cobolVars.add(varCtx.getText());
            }
            selectQuery.setCobolVariables(cobolVars);
        }

        // Extract table name
        selectQuery.setTableName(ctx.tableName().getText());

        // Extract where conditions
        selectQuery.setWhereClause(visitWhereClause(ctx.whereClause()));

        //Order by clause
        selectQuery.setOrderByClause(visitOrderByClause(ctx.orderByClause()));

        return selectQuery;
    }

    @Override
    public WhereClause visitWhereClause(CobolWithSqlParser.WhereClauseContext ctx) {
        WhereClause ret = null;
        if(ctx != null) {
            if (ctx.conditionExpression() != null) {
                ConditionExpression conditionExpression = visitConditionExpression(ctx.conditionExpression());
                ret = new WhereClause(conditionExpression);
            }
            if (ctx.cursorCondition() != null) {
                visitCursorCondition(ctx.cursorCondition());
            }
        }
        return ret;
    }

    @Override
    public SqlStatement visitCursorCondition(CobolWithSqlParser.CursorConditionContext ctx) {
        //TODO
        return super.visitCursorCondition(ctx);
    }

    @Override
    public ConditionExpression visitConditionExpression(CobolWithSqlParser.ConditionExpressionContext ctx) {
        ConditionExpression ret = null;
        List<CobolWithSqlParser.ConditionExpressionContext> conditionExpressionContexts = ctx.conditionExpression();
        List<CobolWithSqlParser.ConditionContext> condition = ctx.condition();
        if (conditionExpressionContexts != null && conditionExpressionContexts.size() != 0) {
            for (int i = 0; i < conditionExpressionContexts.size(); i++) {
                CobolWithSqlParser.ConditionExpressionContext ctx1 = conditionExpressionContexts.get(i);
                if(ret == null)
                    ret = visitConditionExpression(ctx1);
                else
                    ret = new ConditionExpression(ret, visitConditionExpression(ctx1),ctx1.logicalOperator().get(i-1).getText());
            }
        } else if (condition != null & condition.size() != 0) {
            if(condition.size() == 1)
                ret = new ConditionExpression(visitCondition(ctx.condition(0)),null,null);
            else {
                for (int i = 1; i < condition.size(); i++) {
                    Condition nextCondition = visitCondition(ctx.condition(i));
                    String operator = ctx.logicalOperator(i - 1).getText();
                    if (ret == null) {
                        ret = new ConditionExpression(visitCondition(ctx.condition(i - 1)), nextCondition, operator);
                    } else {
                        ret = new ConditionExpression(ret, nextCondition, operator);
                    }
                }
            }
        }

        return ret;
    }

    @Override
    public Condition visitCondition(CobolWithSqlParser.ConditionContext ctx) {
        if (ctx.columnName() != null && ctx.operator() != null) {
            String columnName = ctx.columnName().getText();
            String operator = ctx.operator().getText();
            String value = ctx.value().getText();
            return new SimpleCondition(columnName, operator, value);
        } else if (ctx.columnName() != null && ctx.getText().contains("IS NULL")) {
            String columnName = ctx.columnName().getText();
            return new IsNullCondition(columnName);
        } else if (ctx.columnName() != null && ctx.getText().contains("IS NOT NULL")) {
            String columnName = ctx.columnName().getText();
            return new IsNotNullCondition(columnName);
        }
        return null;
    }

    @Override
    public OrderByClause visitOrderByClause(CobolWithSqlParser.OrderByClauseContext ctx) {
        if(ctx == null)
            return null;
        List<OrderByColumn> columns = new ArrayList<>();

        for (CobolWithSqlParser.OrderByColumnContext columnCtx : ctx.orderByColumn()) {
            SqlStatement column = visitOrderByColumn(columnCtx);
            columns.add((OrderByColumn) column);
        }

        return new OrderByClause(columns);
    }

    @Override
    public OrderByColumn visitOrderByColumn(CobolWithSqlParser.OrderByColumnContext ctx) {
        String columnName = ctx.columnName().getText();

        // Default direction is ASC if not provided
        String direction = ctx.orderDirection() != null ? ctx.orderDirection().getText() : "ASC";

        return new OrderByColumn(columnName, direction);
    }

    @Override
    public SqlStatement visitInsertQuery(CobolWithSqlParser.InsertQueryContext ctx) {
        InsertQuery insertQuery = new InsertQuery();

        // Extract table name
        insertQuery.setTableName(ctx.tableName().getText());

        // Extract columns
        List<String> columns = new ArrayList<>();
        if(ctx.columnList() != null){
            for (CobolWithSqlParser.ColumnNameContext columnCtx : ctx.columnList().columnName()) {
                columns.add(columnCtx.getText());
            }
        }
        insertQuery.setColumns(columns);

        // Extract values
        List<String> values = new ArrayList<>();
        for (CobolWithSqlParser.ValueContext valueCtx : ctx.valueList().value()) {
            values.add(valueCtx.getText());
        }
        insertQuery.setValues(values);

        return insertQuery;
    }

    @Override
    public SqlStatement visitUpdateQuery(CobolWithSqlParser.UpdateQueryContext ctx) {
        UpdateQuery updateQuery = new UpdateQuery();

        // Extract table name
        updateQuery.setTableName(ctx.tableName().getText());

        // Extract assignments
        List<Assignment> assignments = new ArrayList<>();
        CobolWithSqlParser.AssignmentListContext assignmentListContext = ctx.assignmentList();
        List<CobolWithSqlParser.ColumnNameContext> columnNameContexts = assignmentListContext.columnName();
        List<CobolWithSqlParser.ValueContext> value = assignmentListContext.value();
        int count = columnNameContexts.size();
        for (int i = 0; i < count; i++) {

            Assignment assignment = new Assignment();
            assignment.setColumnName(columnNameContexts.get(i).getText());
            assignment.setValue(value.get(i).getText());
            assignments.add(assignment);
        }
        updateQuery.setAssignments(assignments);

        // Extract where conditions
        updateQuery.setWhereClause(visitWhereClause(ctx.whereClause()));

        return updateQuery;
    }

    @Override
    public SqlStatement visitDeleteQuery(CobolWithSqlParser.DeleteQueryContext ctx) {
        DeleteQuery deleteQuery = new DeleteQuery();

        // Extract table name
        deleteQuery.setTableName(ctx.tableName().getText());

        // Extract where conditions

        deleteQuery.setWhereClause(visitWhereClause(ctx.whereClause()));

        return deleteQuery;
    }
    @Override
    public CursorOperation visitOpenCursor(CobolWithSqlParser.OpenCursorContext ctx) {
        String cursorName = ctx.cursorName().getText();
        CursorOperation ret =  new CursorOperation();
        ret.setCursorName(cursorName);
        ret.setOperationType(CursorOperation.OperationType.OPEN);
        return ret;
    }

    @Override
    public CursorOperation visitCloseCursor(CobolWithSqlParser.CloseCursorContext ctx) {
        String cursorName = ctx.cursorName().getText();
        CursorOperation ret =  new CursorOperation();
        ret.setCursorName(cursorName);
        ret.setOperationType(CursorOperation.OperationType.CLOSE);
        return ret;
    }

    @Override
    public CursorOperation visitFetchCursor(CobolWithSqlParser.FetchCursorContext ctx) {
        String cursorName = ctx.cursorName().getText();
        List<String> cobolVariables = ctx.cobolVariableList().cobolVariable().stream()
                .map(v -> v.getText())
                .collect(Collectors.toList());
        CursorOperation ret = new CursorOperation();
        ret.setCursorName(cursorName);
        ret.setOperationType(CursorOperation.OperationType.FETCH);
        ret.setCobolVariables(cobolVariables);
        return ret;
    }
    public static void main(String[] args) throws Exception {
        // 输入为 CobolWithSql 语法的输入字符串
        String input = "EXEC SQL SELECT STUDENT-ID, STUDENT-NAME INTO :WS-STUDENT-ID, :WS-STUDENT-NAME FROM STUDENT WHERE STUDENT-ID = 1004 END-EXEC.";

        // 使用 ANTLR 构建解析器
        CobolWithSqlLexer lexer = new CobolWithSqlLexer(CharStreams.fromString(input));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CobolWithSqlParser parser = new CobolWithSqlParser(tokens);

        // 生成语法树
        ParseTree tree = parser.cobolSql();

        // 使用 Visitor 遍历语法树
        CobolSqlVisitor visitor = new CobolSqlVisitor();
        SqlStatement statement = visitor.visit(tree);

        // 输出生成的 SQL 语句对象
        System.out.println(statement);
    }
}