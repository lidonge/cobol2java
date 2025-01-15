package free.optimize;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.type.Type;

import java.util.List;
import java.util.Objects;
import java.util.Stack;

/**
 * @author lidong@date 2024-12-12@version 1.0
 */
public class ApiField {
    private String qualifiedName;
    private Type type;
    private NodeList<AnnotationExpr> annotationExprs;
    private boolean isIn;
    private boolean isOut;
    private String inStmt;
    private String outStmt;
    private List<BasicAttributesExtractor.NameAndClass> nameAndClassList;

    public ApiField(String name, Type type, NodeList<AnnotationExpr> annotationExprs) {
        this.qualifiedName = name;
        this.type = type;
        this.annotationExprs = annotationExprs;
    }

    public String getShortQualifiedName(){
        return qualifiedName.replace(".", "");
    }
    public List<BasicAttributesExtractor.NameAndClass> getNameAndClassList() {
        return nameAndClassList;
    }

    public void setNameAndClassList(Stack<BasicAttributesExtractor.NameAndClass> stack){
        this.nameAndClassList = stack.stream().toList();
    }
    public String getApiFieldName() {
        String[] parts = qualifiedName.split("\\.");
        String ret = "";
        for (int i = 0; i < parts.length; i++) {
            String str = parts[i];
            ret += str.substring(0, 1).toUpperCase() + str.substring(1);
        }
        return  ret;
    }
    public String getInStmt() {
        return inStmt;
    }

    public void setInStmt(String inStmt) {
        this.inStmt = inStmt;
    }

    public String getOutStmt() {
        return outStmt;
    }

    public void setOutStmt(String outStmt) {
        this.outStmt = outStmt;
    }

    public boolean isIn() {
        return isIn;
    }

    public void setIn(boolean in) {
        isIn = in;
    }

    public boolean isOut() {
        return isOut;
    }

    public void setOut(boolean out) {
        isOut = out;
    }

    public String getQualifiedName() {
        return qualifiedName;
    }

    public Type getType() {
        return type;
    }

    public NodeList<AnnotationExpr> getAnnotationExprs() {
        return annotationExprs;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ApiField apiField = (ApiField) o;
        return Objects.equals(qualifiedName, apiField.qualifiedName) && Objects.equals(type, apiField.type) && Objects.equals(annotationExprs, apiField.annotationExprs);
    }

    @Override
    public int hashCode() {
        return Objects.hash(qualifiedName, type, annotationExprs);
    }

    @Override
    public String toString() {
        return "ApiField{" +
                "qualifiedName='" + qualifiedName + '\'' +
                ", type=" + type +
                ", annotationExprs=" + annotationExprs +
                ", isIn=" + isIn +
                ", isOut=" + isOut +
                '}';
    }
}
