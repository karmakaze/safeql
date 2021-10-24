package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.predicate.Equal;
import org.keithkim.safeql.query.Join;
import org.keithkim.safeql.type.UnsafeString;
import org.keithkim.safeql.util.StringHelpers;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;

@EqualsAndHashCode
public class Expr<T> {
    private static final Pattern NAME_PATTERN = Pattern.compile(":([a-z][A-Za-z_0-9]*)");
    private static final Pattern GROUPED_NAME_PATTERN = Pattern.compile("\\(*:([a-z][A-Za-z_0-9]*)\\)*");
    private static final Pattern TERM_PATTERN = Pattern.compile(":?['A-Za-z0-9][:.'A-Za-z0-9_]*");
    private static final Pattern ONE_GROUP_PATTERN = Pattern.compile("[(][^()]+[)]");

    static final AtomicInteger OBJECT_ID_GENERATOR = new AtomicInteger();

    @EqualsAndHashCode.Exclude
    final int objectId;

    @EqualsAndHashCode.Exclude
    private volatile int varId;

    volatile String sql;
    final Map<String, Object> binds;
    final Map<String, Object> localBinds;

    public static <T> Expr<T> expr(String string) {
        return new Expr<>(string);
    }

    public static <T> Expr<T> expr(String sql, Object... values) {
        return expr(sql, asList(values));
    }

    public static <T> Expr<T> expr(String sql, List<Object> values) {
        Expr<T> expr = new Expr<>(sql);
        for (Object value : values) {
            expr.bindLocal(value);
        }
        return expr;
    }

    public static <T> Expr<T> expr(String sql, Map<String, Object> localBinds) {
        return new Expr<T>(sql).bindLocal(localBinds);
    }

    public Expr(String sql) {
        this(sql, emptyMap());
    }

    public Expr(String sql, Map<String, Object> localBinds) {
        this.sql = sql;
        this.objectId = OBJECT_ID_GENERATOR.incrementAndGet();
        this.localBinds = new TreeMap<>();
        Map<String, Object> parsedBindVars = parseSqlVars(sql);
        if (localBinds.isEmpty()) {
            this.binds = parsedBindVars;
        } else {
            this.binds = new TreeMap<>();
            parsedBindVars.forEach((name, value) -> {
                if (localBinds.containsKey(name)) {
                    bindLocal1(name, value);
                } else {
                    this.binds.put(name, null);
                }
            });
        }
    }

    private Map<String, Object> parseSqlVars(String sql) {
        Map<String, Object> binds = new TreeMap<>();
        if (sql != null) {
            Matcher matcher = NAME_PATTERN.matcher(sql);
            while (matcher.find()) {
                binds.put(matcher.group(1), null);
            }
        }
        return binds;
    }

    public String sql() {
        return sql;
    }

    public void bind(String name, Object value) {
        if (binds.containsKey(name)) {
            bindTo(binds, name, value);
        }
    }

    protected Expr<T> bindLocal(Object value) {
        return bindLocal("?", value);
    }

    protected Expr<T> bindLocal(List<Object> values) {
        values.forEach(value -> bindLocal("?", value));
        return this;
    }

    public Expr<T> bindLocal(Map<String, Object> binds) {
        binds.forEach(this::bindLocal);
        return this;
    }

    public Expr<T> bindLocal(String name, Object value) {
        if ("?".equals(name)) {
            bindLocal1(name, value);
        } else {
            if (binds.containsKey(name)) {
                bindLocal1(name, value);
                binds.remove(name);
            } else if (localBinds.containsKey(name +"_"+ objectId)) {
                bindTo(localBinds, name +"_"+ objectId, value);
            }
        }
        return this;
    }

    private void bindLocal1(String name, Object value) {
        if (StringHelpers.isEmpty(sql)) {
            return;
        }
        if (value instanceof Expr) {
            Expr<?> expr = (Expr) value;
            if ("?".equals(name)) {
                sql = sql.replaceFirst("\\?", expr.sql());
            } else {
                sql = sql.replaceAll(":"+ name +"\\b", expr.sql());
            }
            return;
        }

        final String bindName;
        if ("?".equals(name)) {
            bindName = "_" + ++varId + "_" + objectId;
            sql = sql.replaceFirst("\\?", ":" + bindName);
        } else {
            bindName = name +"_"+ objectId;
            sql = sql.replaceAll(":"+ name +"\\b", ":"+ bindName);
        }

        if (value instanceof UnsafeString) {
            localBinds.put(bindName, ((UnsafeString) value).inject());
        } else {
            localBinds.put(bindName, value);
        }
    }

    public Map<String, ?> binds() {
        return binds;
    }

    public Map<String, ?> localBinds() {
        return localBinds;
    }

    private void bindTo(Map<String, Object> binds, String name, Object value) {
        if (value instanceof UnsafeString) {
            binds.put(name, ((UnsafeString) value).inject());
        } else {
            binds.put(name, value);
        }
    }

    @Override
    public String toString() {
        String bind = "";
        Map<String, ?> allBinds = allBinds();
        if (!allBinds.isEmpty()) {
            bind = " BIND: " + Joiner.on(", ").withKeyValueSeparator(":").join(allBinds);
        }
        return String.format("<SQL: %s;%s>", sql(), bind);
    }

    public Map<String, ?> allBinds() {
        if (!binds.isEmpty() && !localBinds.isEmpty()) {
            SortedMap<String, Object> allBinds = new TreeMap<>(binds);
            allBinds.putAll(localBinds);
            return allBinds;
        } else if (!binds.isEmpty()) {
            return binds;
        } else if (!localBinds.isEmpty()) {
            return localBinds;
        }
        return emptyMap();
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

    public static String grouped(String sql) {
        if (ONE_GROUP_PATTERN.matcher(sql).matches()) {
            return sql;
        }
        return "(" + sql + ")";
    }

    public static String group(Expr<?> expr) {
        if (expr instanceof Equal || expr instanceof Join.Equate) {
            return expr.sql();
        }
        return group(expr.sql());
    }

    public static String group(String sql) {
        if (TERM_PATTERN.matcher(sql).matches() || ONE_GROUP_PATTERN.matcher(sql).matches()) {
            return sql;
        }
        return "(" + sql + ")";
    }
}
