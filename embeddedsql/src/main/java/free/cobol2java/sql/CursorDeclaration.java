package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class CursorDeclaration extends SqlStatement{
    public String cursorName;
    public SelectQuery selectQuery;
    public boolean forUpdate;

    public CursorDeclaration(String cursorName, SelectQuery selectQuery, boolean forUpdate) {
        this.cursorName = cursorName;
        this.selectQuery = selectQuery;
        this.forUpdate = forUpdate;
    }

    @Override
    public String toString() {
        return "CursorDeclaration{" +
                "cursorName='" + cursorName + '\'' +
                ", selectQuery=" + selectQuery +
                ", forUpdate=" + forUpdate +
                '}';
    }
}
