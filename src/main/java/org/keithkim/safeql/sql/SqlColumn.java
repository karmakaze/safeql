package org.keithkim.safeql.sql;

import org.keithkim.safeql.template.Expr;

public class SqlColumn<E extends SqlEntity, T> extends Expr<T> {
    public final E entity;
    public final String columnName;

    public SqlColumn(E entity, String columnName) {
        super(columnName);
        this.entity = entity;
        this.columnName = columnName;
    }
}
