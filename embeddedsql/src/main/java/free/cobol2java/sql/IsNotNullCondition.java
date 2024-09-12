package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class IsNotNullCondition extends Condition {
    public String columnName;

    public IsNotNullCondition(String columnName) {
        this.columnName = columnName;
    }

    @Override
    public String toString() {
        return "IsNotNullCondition{" +
                "columnName='" + columnName + '\'' +
                '}';
    }
}

