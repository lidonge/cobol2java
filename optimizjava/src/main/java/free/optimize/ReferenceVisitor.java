package free.optimize;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.visitor.VoidVisitorAdapter;

import java.util.List;
import java.util.Set;

/**
 * @author lidong@date 2024-12-06@version 1.0
 */
public class ReferenceVisitor extends VoidVisitorAdapter<Void> {
    Set<String> fieldNames;
    Set<String> referencedFields;
    List<FieldDeclaration> allFieldDeclarations;

    public ReferenceVisitor(Set<String> fieldNames, Set<String> referencedFields, List<FieldDeclaration> allFieldDeclarations) {
        this.fieldNames = fieldNames;
        this.referencedFields = referencedFields;
        this.allFieldDeclarations = allFieldDeclarations;
    }

    @Override
    public void visit(NameExpr nameExpr, Void arg) {
        super.visit(nameExpr, arg);
        String fieldName = nameExpr.getNameAsString();

        // If the field name is in the defined fields
        if (fieldNames.contains(fieldName)) {
            if (!isPartOfAssignmentLeftSide(nameExpr, fieldName)) {
                // If the field is not on the left side of an assignment, mark it as referenced
                referencedFields.add(fieldName);
                markObjectAndAttributesAsReferenced(fieldName);
            }
        }
    }

    @Override
    public void visit(FieldAccessExpr fieldAccessExpr, Void arg) {
        super.visit(fieldAccessExpr, arg);
        String fieldName = fieldAccessExpr.getNameAsString();

        // If the field name is in the defined fields
        if (fieldNames.contains(fieldName)) {
            // Recursively check the parent node chain
            if (!isPartOfAssignmentLeftSide(fieldAccessExpr, fieldName)) {
                referencedFields.add(fieldName);
                markObjectAndAttributesAsReferenced(fieldName);
            }
        }
    }

    /**
     * Recursively check if the node is part of the left-hand side of an assignment
     * @param node The current node
     * @return true if it is part of the left-hand side, false otherwise
     */
    private boolean isPartOfAssignmentLeftSide(Node node, String fieldName) {
        if (node == null) {
            return false;
        }

        Node parent = node.getParentNode().orElse(null);

        // If the parent node is an AssignExpr and the target is the current node, return true
        if (parent instanceof AssignExpr) {
            AssignExpr assignExpr = (AssignExpr) parent;
            return assignExpr.getTarget().toString().indexOf(fieldName) != -1;
        }

        // Recursively check the parent node
        return isPartOfAssignmentLeftSide(parent, fieldName);
    }

    /**
     * Recursively mark the object and all its attributes as referenced
     * @param objectName The name of the object
     */
    private void markObjectAndAttributesAsReferenced(String objectName) {
        if (!fieldNames.contains(objectName)) return;

        // Mark the object itself as referenced
        referencedFields.add(objectName);

        // Get the fields (attributes) of the object
        for (FieldDeclaration field : allFieldDeclarations) {
            for (VariableDeclarator variable : field.getVariables()) {
                if (variable.getNameAsString().equals(objectName)) {
                    // If it is a composite object, recursively mark its attributes
                    if (variable.getType().isClassOrInterfaceType()) {
                        String className = variable.getType().asClassOrInterfaceType().getNameAsString();
                        markAttributesOfClass(className);
                    }
                }
            }
        }
    }

    /**
     * Mark all attributes of a given class as referenced
     * @param className The name of the class
     */
    private void markAttributesOfClass(String className) {
        for (FieldDeclaration field : allFieldDeclarations) {
            for (VariableDeclarator variable : field.getVariables()) {
                if (variable.getType().isClassOrInterfaceType() &&
                        variable.getType().asClassOrInterfaceType().getNameAsString().equals(className)) {
                    referencedFields.add(variable.getNameAsString());
                }
            }
        }
    }
}