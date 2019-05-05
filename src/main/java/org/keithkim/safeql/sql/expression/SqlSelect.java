package org.keithkim.safeql.sql.expression;

import com.google.common.base.Joiner;
import org.keithkim.safeql.template.Expr;
import org.keithkim.safeql.util.System;

import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.toList;

public class SqlSelect<E extends SqlEntity> extends Expr<SqlRows<E>> {
    private final SqlTable table;
    private final List<SqlTable<E>.SqlColumn<?>> columns;

    public SqlSelect(SqlTable<E> table, List<SqlTable<E>.SqlColumn<?>> columns) {
        super(null);
        this.table = table;
        this.columns = columns;
    }

    public <E extends SqlEntity> SqlTable<E> asTableExpr() {
        return asTableExpr(null);
    }
    public <E extends SqlEntity> SqlTable<E> asTableExpr(String alias) {
        return new SqlTable<E>(table.entityClass, resolve(emptyMap()).toString(), alias);
    }

    public Expr<SqlRows<E>> resolve(Map<String, ?> params) {
        List<String> cols = columns.stream().map(c -> c.selectTerm(params)).collect(toList());
        String fromTable = "";
        if (table != System.Table.none) {
            fromTable = " FROM "+ table.resolve(params);
        }
        return Expr.expr(String.format("SELECT %s" + fromTable, Joiner.on(", ").join(cols)));
    }
}
