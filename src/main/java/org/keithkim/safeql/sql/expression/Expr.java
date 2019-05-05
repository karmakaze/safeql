package org.keithkim.safeql.sql.expression;

import com.google.common.base.Joiner;

import java.util.Map;
import java.util.TreeMap;

public class Expr<T> {
    final String sql;
    final Map<String, Object> binds = new TreeMap<>();

    public static <T> Expr<T> expr(String string) {
        return new Expr<>(string);
    }

    public Expr(String sql) {
        this.sql = sql;
    }

    public String sql() {
        return sql;
    }

    public <V> void bind(String name, V value) {
        binds.put(name, value);
    }

    public Map<String, ?> binds() {
        return binds;
    }

    public String toString() {
        String bind = "";
        if (!binds.isEmpty()) {
            bind = " BIND: " + Joiner.on(", ").withKeyValueSeparator(":").join(binds);
        }
        return String.format("<SQL: %s;%s>", sql, bind);
    }
}
