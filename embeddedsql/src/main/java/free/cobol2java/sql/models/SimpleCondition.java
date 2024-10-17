package free.cobol2java.sql.models;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class SimpleCondition extends Condition {
    public String columnName;
    public String operator;
    public String value;

    public SimpleCondition(String columnName, String operator, String value) {
        this.columnName = columnName;
        this.operator = operator;
        this.value = value;
    }

    @Override
    public String toString() {
        return "SimpleCondition{" +
                "columnName='" + columnName + '\'' +
                ", operator='" + operator + '\'' +
                ", value='" + value + '\'' +
                '}';
    }
}