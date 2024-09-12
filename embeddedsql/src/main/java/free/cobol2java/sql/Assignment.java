package free.cobol2java.sql;

import java.util.List;

// Assignment object for UPDATE query
public class Assignment {
    private String columnName;
    private String value;

    // Constructor and getters/setters
    public Assignment() {
    }
    public String getColumnName() {
        return columnName;
    }
    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }
    public String getValue() {
        return value;
    }
    public void setValue(String value) {
        this.value = value;
    }
}
