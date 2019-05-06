package org.keithkim.safeql.statement;

import com.google.common.base.Joiner;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;

import java.util.List;
import java.util.Optional;

import static java.util.Collections.singletonList;

public class SelectStatement<E extends Entity> implements QueryStatement {
    private final Class<E> entityClass;
    private final List<Table.SqlColumn> columns;
    private final List<Table<? extends Entity>> tables;
    private final Optional<Expr<Boolean>> whereCond;

    public SelectStatement(Class<E> entityClass, List<Table.SqlColumn> columns, Table<E> table) {
        this.entityClass = entityClass;
        this.columns = columns;
        this.tables = singletonList(table);
        this.whereCond = Optional.empty();
    }

    public SelectStatement(Class<E> entityClass, List<Table.SqlColumn> columns, List<Table<? extends Entity>> tables,
                           Expr<Boolean> whereCond) {
        this.entityClass = entityClass;
        this.columns = columns;
        this.tables = tables;
        this.whereCond = Optional.ofNullable(whereCond);
    }

    @Override
    public Class<E> entityClass() {
        return entityClass;
    }

    @Override
    public List<Table<? extends Entity>> tables() {
        return tables;
    }

    @Override
    public String sql() {
        String whereClause = "";
        if (whereCond.isPresent()) {
            whereClause = "WHERE " + whereCond.toString();
        }
        return String.format("SELECT %s FROM %s"+whereClause, Joiner.on(", ").join(columns), Joiner.on(", ").join(tables));
    }
}
