package free.cobol2java.sql;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class WhereQuery extends SqlStatement{
    private WhereClause whereClause;
    private OrderByClause orderByClause;

    public WhereClause getWhereClause() {
        return whereClause;
    }

    public void setWhereClause(WhereClause whereClause) {
        this.whereClause = whereClause;
    }

    public OrderByClause getOrderByClause() {
        return orderByClause;
    }

    public void setOrderByClause(OrderByClause orderByClause) {
        this.orderByClause = orderByClause;
    }
}
