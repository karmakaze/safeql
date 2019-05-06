package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;

import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Expr<T> {
    private static final Pattern GROUPED_NAME_PATTERN = Pattern.compile("\\(*:([A-Za-z_][A-Za-z_0-9]*)\\)*");
    private static final Pattern TERM_PATTERN = Pattern.compile("[':.A-Za-z0-9_]+");
    private static final Pattern ONE_GROUP_PATTERN = Pattern.compile("[(][^()]+[)]");

    final String sql;
    final Map<String, Object> binds = new TreeMap<>();

    public static <T> Expr<T> expr(String string) {
        return new Expr<>(string);
    }

    public static String grouped(String string) {
        if (ONE_GROUP_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }

    public static String group(String string) {
        if (TERM_PATTERN.matcher(string).matches() || ONE_GROUP_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }

    public Expr(String sql) {
        this.sql = sql;
    }

    public String sql() {
        return sql;
    }

    public void bind(String name, Object value) {
        binds.put(name, value);
    }

    public Map<String, ?> binds() {
        return binds;
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof Expr)) {
            return false;
        }
        Expr that = (Expr) other;
        return Objects.equals(this.sql, that.sql) && Objects.equals(this.binds, that.binds);
    }

    public int hashCode() {
        return Objects.hashCode(sql) * 997 + Objects.hashCode(binds) * 991;
    }

    public String toString() {
        String bind = "";
        if (!binds.isEmpty()) {
            bind = " BIND: " + Joiner.on(", ").withKeyValueSeparator(":").join(binds);
        }
        return String.format("<SQL: %s;%s>", sql(), bind);
    }

    protected Object eval() {
        String sql = sql();
        Matcher matcher = GROUPED_NAME_PATTERN.matcher(sql);
        if (matcher.matches()) {
            String name = matcher.group(1);
            return binds().get(name);
        }
        return null;
    }
}
