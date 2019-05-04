package org.keithkim.safeql;

import org.keithkim.safeql.template.Expr;

import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SelectQuery extends Expr<String> {
    private static final Pattern varPattern = Pattern.compile(":[A-Za-z][A-Za-z0-9_]*");

    public SelectQuery(String sql) {
        super(sql);
        Matcher matcher = varPattern.matcher(sql);
        Map<String, Optional<?>> params = params();
        while (matcher.find()) {
            String var = matcher.group();
            params.putIfAbsent(var.substring(1), Optional.empty());
        }
    }

    public Expr<String> resolve(Map<String, ?> params) {
        return Expr.expr(super.toString());
    }

    public String toString() {
        return super.toString();
    }
}
