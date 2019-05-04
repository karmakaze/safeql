package org.keithkim.safeql.sql;

import org.keithkim.safeql.template.Expr;

import java.util.Map;
import java.util.Optional;

import static org.keithkim.safeql.template.Helpers.group;

public class SqlTableAlias<T extends SqlEntity> extends SqlTable<T> implements SqlAlias<T> {
    private final String alias;

    public SqlTableAlias(Class<T> entityClass, String alias) {
        this(entityClass, entityClass.getSimpleName().toLowerCase(), alias);
    }

    public SqlTableAlias(Class<T> entityClass, String expr, String alias) {
        super(entityClass, expr);
        this.alias = alias;
    }

    public Expr<T> resolve(Map<String, ?> params) {
        return Expr.expr(group(super.resolve(params).toString()) +" "+ alias);
    }

    public Optional<String> alias() {
        return Optional.of(alias);
    }
}
