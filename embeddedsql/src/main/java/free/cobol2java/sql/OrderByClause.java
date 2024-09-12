package free.cobol2java.sql;

import java.util.List;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */
public class OrderByClause extends SqlStatement{
    public List<OrderByColumn> columns;

    public OrderByClause(List<OrderByColumn> columns) {
        this.columns = columns;
    }

    @Override
    public String toString() {
        return "OrderByClause{" +
                "columns=" + columns +
                '}';
    }
}

