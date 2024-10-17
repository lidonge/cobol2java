package free.cobol2java.sql.models;

import java.util.List;

// SQL SELECT statement object
public class SelectQuery extends WhereQuery {
    private boolean allColumn = false;
    private List<String> columns;
    private List<String> cobolVariables;
    private String tableName;
    private OrderByClause orderByClause;
    // Constructor and getters/setters

    public OrderByClause getOrderByClause() {
        return orderByClause;
    }

    public void setOrderByClause(OrderByClause orderByClause) {
        this.orderByClause = orderByClause;
    }

    public List<String> getColumns() {
        return columns;
    }

    public void setColumns(List<String> columns) {
        this.columns = columns;
    }

    public List<String> getCobolVariables() {
        return cobolVariables;
    }

    public void setCobolVariables(List<String> cobolVariables) {
        this.cobolVariables = cobolVariables;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public boolean isAllColumn() {
        return allColumn;
    }

    public void setAllColumn(boolean allColumn) {
        this.allColumn = allColumn;
    }
}
