package org.keithkim.safeql.sql.expression;

import java.util.regex.Pattern;

public class SqlRawSelect<E extends SqlEntity> extends Expr<SqlRows<E>> {
    private static final Pattern varPattern = Pattern.compile(":[A-Za-z][A-Za-z0-9_]*");
    private final Class<E> entityClass;

    public SqlRawSelect(Class<E> entityClass, String sql) {
        super(sql);
        this.entityClass = entityClass;
    }
}
