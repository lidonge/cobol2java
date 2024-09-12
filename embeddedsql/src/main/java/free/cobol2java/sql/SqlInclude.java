package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class SqlInclude extends SqlStatement{
    private String includeFile;
    private String includeId;

    public String getIncludeId() {
        return includeId;
    }

    public void setIncludeId(String includeId) {
        this.includeId = includeId;
    }

    public String getIncludeFile() {
        return includeFile;
    }

    public void setIncludeFile(String includeFile) {
        this.includeFile = includeFile;
    }
}
