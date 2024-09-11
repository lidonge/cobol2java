package free.cobol2java.copybook;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public class ModelConfig {
    private String name;
    private List<FieldConfig> fields = new ArrayList<>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<FieldConfig> getFields() {
        return fields;
    }

    public void setFields(List<FieldConfig> fields) {
        this.fields = fields;
    }
}
