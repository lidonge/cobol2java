package free.cobol2java.sql.models;

// SQL DELETE statement object
public class DeleteQuery extends WhereQuery {
    private String tableName;

    // Constructor and getters/setters

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

}
