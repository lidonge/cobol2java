package free.optimize;

import java.util.ArrayList;
import java.util.List;
/**
 * @author lidong@date 2024-12-09@version 1.0
 */
public class ApiBizParameter {
    private String bizId;
    private List<String> parameters = new ArrayList<>();
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

    public List<String> getParameters() {
        return parameters;
    }

    public void addParameter(String parameter) {
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
