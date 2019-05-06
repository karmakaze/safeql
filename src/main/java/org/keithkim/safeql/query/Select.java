package org.keithkim.safeql.query;

import com.google.common.base.Joiner;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Sys;
import org.keithkim.safeql.schema.Table;
import org.keithkim.safeql.type.Rows;

import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toList;

public class Select<E extends Entity> extends Expr<Rows<E>> {
    private final Table table;
    private final List<Table<E>.SqlColumn<?>> columns;

    public Select(Table<E> table, List<Table<E>.SqlColumn<?>> columns) {
        super(null);
        this.table = table;
        this.columns = columns;
    }

    public <E extends Entity> Table<E> asTableExpr() {
        return asTableExpr(null);
    }
    public <E extends Entity> Table<E> asTableExpr(String alias) {
        return new Table<E>(table.entityClass, sql(), alias);
    }

    public String sql() {
        Map<String, ?> binds = binds();
        List<String> cols = columns.stream().map(c -> c.selectTerm(binds)).collect(toList());
        String fromTable = "";
        if (table != Sys.Table.none) {
            fromTable = " FROM "+ table.sql();
        }
        return String.format("SELECT %s" + fromTable, Joiner.on(", ").join(cols));
    }
}
