package free.cobol2java.sql.models;

import java.util.List;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class CursorOperation extends SqlStatement{
    private String cursorName;
    private OperationType operationType;
    private List<String> cobolVariables; // 对于 FETCH 操作

    public enum OperationType {
        OPEN,
        CLOSE,
        FETCH
    }

    public String getCursorName() {
        return cursorName;
    }

    public void setCursorName(String cursorName) {
        this.cursorName = cursorName;
    }

    public OperationType getOperationType() {
        return operationType;
    }

    public void setOperationType(OperationType operationType) {
        this.operationType = operationType;
    }

    public List<String> getCobolVariables() {
        return cobolVariables;
    }

    public void setCobolVariables(List<String> cobolVariables) {
        this.cobolVariables = cobolVariables;
    }
}