package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */

public class IsNullCondition extends Condition {
    public String columnName;

    public IsNullCondition(String columnName) {
        this.columnName = columnName;
    }

    @Override
    public String toString() {
        return "IsNullCondition{" +
                "columnName='" + columnName + '\'' +
                '}';
    }
}
