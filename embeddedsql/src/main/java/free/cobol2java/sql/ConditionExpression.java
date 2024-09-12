package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */

public class ConditionExpression extends Condition{
    public Condition left;
    public Condition right;
    public String logicalOperator; // AND or OR

    public ConditionExpression(Condition left, Condition right, String logicalOperator) {
        this.left = left;
        this.right = right;
        this.logicalOperator = logicalOperator;
    }

    @Override
    public String toString() {
        return "ConditionExpression{" +
                "left=" + left +
                ", right=" + right +
                ", logicalOperator='" + logicalOperator + '\'' +
                '}';
    }
}