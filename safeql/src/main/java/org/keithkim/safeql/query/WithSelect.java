package org.keithkim.safeql.query;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;

import java.util.List;

import static java.util.Arrays.asList;

@EqualsAndHashCode(callSuper = false)
public class WithSelect<E extends Entity> extends Select<E> {
    private final With with;

    public WithSelect(With with, Table<E> table, Table<E>.SqlColumn<?>... columns) {
        this(with, table, asList(columns));
    }

    public WithSelect(With with, Table<E> table, List<Table<E>.SqlColumn<?>> columns) {
        super(table, columns);
        this.with = with;
    }

    public String sql() {
        String withSql = with.sql();
        if (!withSql.isEmpty()) {
            withSql += '\n';
        }
        return withSql + super.sql();
    }
}
