package org.keithkim.safeql.sql;

import com.google.common.collect.Table;
import org.keithkim.safeql.template.Expr;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Collections.emptyList;

public class SqlRawSelect<E extends SqlEntity> extends Expr<SqlRows<E>> implements SqlStatement<E> {
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

    @Override
    public List<E> queryList() {
        return Registry.using(emptyList(), handle -> {
            handle.createQuery(toString())
                    .mapTo(entityClass)
                    .list();
            return null;
        });
    }

    @Override
    public int execute() {
        return 0;
    }
}
