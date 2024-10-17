package free.cobol2java.sql.models;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
// OrderByColumn model with direction (ASC or DESC)
public class OrderByColumn extends SqlStatement{
    public String columnName;
    public String direction; // ASC or DESC

    public OrderByColumn(String columnName, String direction) {
        this.columnName = columnName;
        this.direction = direction;
    }

    @Override
    public String toString() {
        return "OrderByColumn{" +
                "columnName='" + columnName + '\'' +
                ", direction='" + direction + '\'' +
                '}';
    }
}