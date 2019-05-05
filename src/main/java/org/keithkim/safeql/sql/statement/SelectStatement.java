package org.keithkim.safeql.sql.statement;

import com.google.common.base.Joiner;
import org.keithkim.safeql.sql.expression.Expr;
import org.keithkim.safeql.sql.expression.SqlEntity;
import org.keithkim.safeql.sql.expression.SqlTable;

import java.util.List;
import java.util.Optional;

import static java.util.Collections.singletonList;

public class SelectStatement<E extends SqlEntity> implements QueryStatement {
    private final Class<E> entityClass;
    private final List<SqlTable.SqlColumn> columns;
    private final List<SqlTable<? extends SqlEntity>> tables;
    private final Optional<Expr<Boolean>> whereCond;

    public SelectStatement(Class<E> entityClass, List<SqlTable.SqlColumn> columns, SqlTable<E> table) {
        this.entityClass = entityClass;
        this.columns = columns;
        this.tables = singletonList(table);
        this.whereCond = Optional.empty();
    }

    public SelectStatement(Class<E> entityClass, List<SqlTable.SqlColumn> columns, List<SqlTable<? extends SqlEntity>> tables,
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
    public List<SqlTable<? extends SqlEntity>> tables() {
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
