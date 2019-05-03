package org.keithkim.safeql;

import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SelectQuery extends SqlExpr {
    public final String sql;

    private static final Pattern varPattern = Pattern.compile(":[A-Za-z][A-Za-z0-9_]*");

    public SelectQuery(String sql) {
        this.sql = sql;
        Matcher matcher = varPattern.matcher(sql);
        Map<String, Optional<?>> params = params();
        while (matcher.find()) {
            String var = matcher.group();
            params.putIfAbsent(var.substring(1), Optional.empty());
        }
    }

    public String sql() {
        return sql;
    }

    public String toString() {
        return sql;
    }
}
