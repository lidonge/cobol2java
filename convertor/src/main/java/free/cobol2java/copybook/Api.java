package free.cobol2java.copybook;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public class Api extends Model{
    private String copybookPath;
    private String copybookName;
    private List<FieldConfig> fieldConfigs = new ArrayList<>();

    public String getCopybookPath() {
        return copybookPath;
    }

    public void setCopybookPath(String copybookPath) {
        this.copybookPath = copybookPath;
    }

    public String getCopybookName() {
        return copybookName;
    }

    public void setCopybookName(String copybookName) {
        this.copybookName = copybookName;
    }

    public List<FieldConfig> getFieldConfigs() {
        return fieldConfigs;
    }

    public void setFieldConfigs(List<FieldConfig> fieldConfigs) {
        this.fieldConfigs = fieldConfigs;
    }
}
