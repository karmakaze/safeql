package org.keithkim.safeql.sql;

import org.keithkim.safeql.template.Expr;

import java.util.Optional;

public class SqlTable<T extends SqlEntity> extends Expr<T> {
    public final Class<T> entityClass;

    public SqlTable(Class<T> entityClass) {
        this(entityClass, entityClass.getSimpleName().toLowerCase());
    }

    public SqlTable(Class<T> tableClass, String tableExpr) {
        super(tableExpr);
        this.entityClass = tableClass;
    }

    public Optional<String> alias() {
        return Optional.empty();
    }
}
