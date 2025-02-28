package free.optimize;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @author lidong@date 2024-12-09@version 1.0
 */
public class ApiBizParameter {
    private String bizId;
    private List<Parameter> parameters = new ArrayList<>();
    private String packName;
    private List<String> subBizNames = new ArrayList<>();

    public String getPackName() {
        return packName;
    }

    public void setPackName(String packName) {
        this.packName = packName;
    }

    public String getBizId() {
        return bizId;
    }

    public void setBizId(String bizId) {
        this.bizId = bizId;
    }

    public List<Parameter> getParameters() {
        return parameters;
    }

    public static class Parameter{
        private String name;
        private String type;

        public Parameter(String type, String fieldName) {
            this.type = type;
            this.name = fieldName;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Parameter parameter = (Parameter) o;
            return Objects.equals(name, parameter.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }

        @Override
        public String toString() {
            return "Parameter{" +
                    "name='" + name + '\'' +
                    ", type='" + type + '\'' +
                    '}';
        }
    }

    public void addParameter(Parameter parameter) {
        if(!this.parameters.contains(parameter))
            parameters.add(parameter);
    }

    public void addSubBiz(String name){
        if(!this.subBizNames.contains(name))
            subBizNames.add(name);
    }

    public List<String> getSubBizNames() {
        return subBizNames;
    }

    @Override
    public String toString() {
        return "ApiBizParameter{" +
                "bizId='" + bizId + '\'' +
                ", parameters=" + parameters +
                '}';
    }
}
