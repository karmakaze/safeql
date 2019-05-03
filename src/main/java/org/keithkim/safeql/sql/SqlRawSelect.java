package org.keithkim.safeql.sql;

import org.keithkim.safeql.template.Expr;

import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SqlRawSelect<T extends Rows> extends Expr<T> {
    private static final Pattern varPattern = Pattern.compile(":[A-Za-z][A-Za-z0-9_]*");

    public SqlRawSelect(String sql) {
        super(sql);

        Matcher matcher = varPattern.matcher(sql);
        Map<String, Optional<?>> params = params();
        while (matcher.find()) {
            String var = matcher.group();
            params.putIfAbsent(var.substring(1), Optional.empty());
        }
    }
}