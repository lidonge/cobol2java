package free.cobol2java.sql;

import java.util.List;

// SQL UPDATE statement object
public class UpdateQuery extends WhereQuery {
    private String tableName;
    private List<Assignment> assignments;

    // Constructor and getters/setters

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public List<Assignment> getAssignments() {
        return assignments;
    }

    public void setAssignments(List<Assignment> assignments) {
        this.assignments = assignments;
    }
}
