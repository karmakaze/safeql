package org.keithkim.safeql;

import org.keithkim.safeql.sql.Entity;

public class Table<T extends Entity> extends SqlAliasExpr {
    public final Class<T> tableClass;

    public Table(Class<T> tableClass) {
        this(tableClass, tableClass.getName().toLowerCase());
    }

    public Table(Class<T> tableClass, String alias) {
        super(new RawSqlExpr(tableClass.getSimpleName().toLowerCase()), alias);
        this.tableClass = tableClass;
    }
}
