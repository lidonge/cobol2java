package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */

public class WhereClause extends SqlStatement{
    public ConditionExpression conditionExpression;

    public WhereClause(ConditionExpression conditionExpression) {
        this.conditionExpression = conditionExpression;
    }

    @Override
    public String toString() {
        return "WhereClause{" +
                "conditionExpression=" + conditionExpression +
                '}';
    }
}



