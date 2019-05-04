package org.keithkim.safeql.sql;

import org.keithkim.safeql.template.Expr;

import java.util.Map;
import java.util.Optional;

import static org.keithkim.safeql.template.Helpers.group;

public class SqlColumnAlias<E extends SqlEntity, T> extends SqlColumn<E, T> implements SqlAlias<T> {
    private final String alias;

    public SqlColumnAlias(E entity, String columnName, String alias) {
        super(entity, columnName);
        this.alias = alias;
    }

    public Expr<T> resolve(Map<String, ?> params) {
        return Expr.expr(group(super.resolve(params).toString()) +" "+ alias);
    }

    public Optional<String> alias() {
        return Optional.of(alias);
    }
}
