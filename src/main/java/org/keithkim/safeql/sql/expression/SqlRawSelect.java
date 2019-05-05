package org.keithkim.safeql.sql.expression;

import org.keithkim.safeql.template.Expr;

import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SqlRawSelect<E extends SqlEntity> extends Expr<SqlRows<E>> {
    private static final Pattern varPattern = Pattern.compile(":[A-Za-z][A-Za-z0-9_]*");
    private final Class<E> entityClass;

    public SqlRawSelect(Class<E> entityClass, String sql) {
        super(sql);
        this.entityClass = entityClass;

        Matcher matcher = varPattern.matcher(sql);
        Map<String, Optional<?>> params = params();
        while (matcher.find()) {
            String var = matcher.group();
            params.putIfAbsent(var.substring(1), Optional.empty());
        }
    }
}
