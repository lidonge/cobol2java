package free.cobol2java.sql;

import java.util.List;

// SQL INSERT statement object
public class InsertQuery extends SqlStatement {
    private String tableName;
    private List<String> columns;
    private List<String> values;

    // Constructor and getters/setters

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public List<String> getColumns() {
        return columns;
    }

    public void setColumns(List<String> columns) {
        this.columns = columns;
    }

    public List<String> getValues() {
        return values;
    }

    public void setValues(List<String> values) {
        this.values = values;
    }
}
