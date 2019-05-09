package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.predicate.Equal;
import org.keithkim.safeql.query.Join;
import org.keithkim.safeql.type.UnsafeString;

import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@EqualsAndHashCode
public class Expr<T> {
    private static final Pattern NAME_PATTERN = Pattern.compile(":([A-Za-z_][A-Za-z_0-9]*)");
    private static final Pattern GROUPED_NAME_PATTERN = Pattern.compile("\\(*:([A-Za-z_][A-Za-z_0-9]*)\\)*");
    private static final Pattern TERM_PATTERN = Pattern.compile("[':.A-Za-z0-9_]+");
    private static final Pattern ONE_GROUP_PATTERN = Pattern.compile("[(][^()]+[)]");

    static final AtomicInteger OBJECT_ID_GENERATOR = new AtomicInteger();

    @EqualsAndHashCode.Exclude
    private final int objectId;

    volatile String sql;
    final Map<String, Object> binds;
    final Map<String, Object> localBinds = new TreeMap<>();

    public static <T> Expr<T> expr(String string) {
        return new Expr<>(string);
    }

    public Expr(String sql) {
        this.sql = sql;
        this.objectId = OBJECT_ID_GENERATOR.incrementAndGet();
        this.binds = parseSqlVars(sql);
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

    public void localBind(String name, Object value) {
        if (binds.containsKey(name)) {
            bindLocal(name, value);
            binds.remove(name);
        } else if (localBinds.containsKey(name +"_"+ objectId)) {
            bindTo(localBinds, name +"_"+ objectId, value);
        }
    }

    private void bindLocal(String name, Object value) {
        String bindName = name +"_"+ objectId;
        if (!binds.containsKey(bindName)) {
            sql = sql.replaceAll(":"+ name +"\\b", ":"+ name +"_"+ objectId);
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

    public static String grouped(String string) {
        if (ONE_GROUP_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }

    public static String group(Expr<?> expr) {
        if (expr instanceof Equal || expr instanceof Join.Equate) {
            return expr.sql();
        }
        return group(expr.sql());
    }

    public static String group(String string) {
        if (TERM_PATTERN.matcher(string).matches() || ONE_GROUP_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }
}
