package org.keithkim.safeql.sql;

import com.google.common.base.Joiner;
import org.keithkim.safeql.template.Expr;

import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toList;

public class SqlSelect<E extends SqlEntity> extends Expr<SqlRows<E>> {
    private final SqlTable table;
    private final List<SqlColumn<E, ?>> columns;

    public SqlSelect(SqlTable<E> table, List<SqlColumn<E, ?>> columns) {
        super(null);
        this.table = table;
        this.columns = columns;
    }

    public Expr<SqlRows<E>> resolve(Map<String, ?> params) {
        List<String> cols = columns.stream().map(c -> c.resolve(params).toString()).collect(toList());
        return Expr.expr(String.format("SELECT %s FROM %s", Joiner.on(", ").join(cols), table.resolve(params)));
    }
}
