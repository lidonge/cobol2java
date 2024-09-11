package free.cobol2java.copybook;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public class FieldConfig {
    private String name;
    private Integer length;
    private Integer scale;
    private String initValue;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getLength() {
        return length;
    }

    public void setLength(Integer length) {
        this.length = length;
    }

    public Integer getScale() {
        return scale;
    }

    public void setScale(Integer scale) {
        this.scale = scale;
    }

    public String getInitValue() {
        return initValue;
    }

    public void setInitValue(String initValue) {
        this.initValue = initValue;
    }
}
