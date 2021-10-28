package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.schema.Table;
import org.keithkim.safeql.type.UnsafeString;
import org.keithkim.safeql.util.SequentialIdGenerator;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Arrays.asList;
import static java.util.Collections.emptySet;

@EqualsAndHashCode
public class Expr<S> implements Eval, SqlScalar<S> {
    private static final Pattern NAME_PATTERN = Pattern.compile(":([A-Za-z][A-Za-z0-9_]*|_[1-9][0-9]*(?:_[1-9A-Za-z][0-9A-Za-z]*)?)");
    private static final Pattern GROUPED_NAME_PATTERN = Pattern.compile("\\(*:([a-z][A-Za-z_0-9]*)\\)*");
    private static final Pattern TERM_PATTERN = Pattern.compile(":?['A-Za-z0-9][:.'A-Za-z0-9_]*");
    private static final Pattern ONE_GROUP_PATTERN = Pattern.compile("[(][^()]+[)]");

    protected static final SequentialIdGenerator objectIdGenerator = new SequentialIdGenerator();

    @EqualsAndHashCode.Exclude
    public final String objectId;

    @EqualsAndHashCode.Exclude
    private volatile int varId;

    volatile String sql;
    volatile boolean isTerm;
    final LinkedHashMap<String, Object> localBinds;

    public static <S> Expr<S> expr(String string) {
        return new Expr<>(string);
    }

    public static <S> Expr<S> expr(String sql, Object... values) {
        return expr(sql, false, asList(values));
    }

    public static <S> Expr<S> expr(String sql, boolean isTerm, List<Object> values) {
        SortedMap<String, Object> bindings = new TreeMap<>();
        int i = 0;
        for (Object value : values) {
            String name = "_"+ (++i);
            sql = sql.replaceFirst("\\?", ":"+name);
            bindings.put(name, value);
        }
        return new Expr<>(sql, bindings.entrySet(), true);
    }

    public static <S> Expr<S> expr(String sql, Map<String, Object> localBinds) {
        return new Expr<S>(sql, localBinds.entrySet(), true);
    }

    public Expr(String sql) {
        this(sql, sql != null && TERM_PATTERN.matcher(sql).matches());
    }

    public Expr(String sql, boolean isTerm) {
        this(sql, isTerm, emptySet(), true);
    }

    protected Expr(String sql, Set<Map.Entry<String, Object>> bindEntries, boolean objectNumber) {
        this(sql, sql != null && TERM_PATTERN.matcher(sql).matches(), bindEntries, objectNumber);
    }

    protected Expr(String sql, boolean isTerm, Set<Map.Entry<String, Object>> bindEntries, boolean objectNumber) {
        this.objectId = objectIdGenerator.makeIdString();
        this.isTerm = isTerm;
        this.localBinds = parseSqlVars(sql);
        if (bindEntries != null) {
            for (Map.Entry<String, Object> me : bindEntries) {
                String name = me.getKey();
                if (localBinds.containsKey(name)) {
                    if (objectNumber) {
                        sql = sql.replace(":"+name, ":"+name+"_"+ objectId);
                        localBinds.remove(name);
                        name = name+"_"+ objectId;
                    }
                    localBinds.put(name, me.getValue());
                } else {
                    System.err.printf("No bind name '%s' in SQL: %s\n", name, sql);
                }
            }
        }
        this.sql = sql;
    }

    protected static <S> String expandTermSql(Expr<S> expr) {
        return expr.isTerm() ? expr.sql() : "("+expr.sql()+")";
    }

    public boolean isTerm() {
        return isTerm;
    }

    private LinkedHashMap<String, Object> parseSqlVars(String sql) {
        LinkedHashMap<String, Object> binds = new LinkedHashMap<>();
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
        LinkedHashMap<String, Object> localBinds = localBinds();
        if ("?".equals(name)) {
            int i = sql.indexOf('?');
            if (i >= 0) {
                if (value instanceof Table) {
                    sql = sql.substring(0, i) + ((Table) value).sqlTerm() + sql.substring(i + 1);
                } else {
                    name = "_" + (++varId) + "_" + objectId;
                    if (sql.length() == i + 1 || " ),".indexOf(sql.charAt(i + 1)) >= 0) {
                        sql = sql.substring(0, i) + ":" + name + sql.substring(i + 1);
                    } else {
                        sql = sql.substring(0, i) + ":" + name + " " + sql.substring(i + 1);
                    }
                    bindTo(localBinds, name, value);
                }
            } else {
                System.err.printf("Attempt positional binding without '?' in SQL: %s\n", sql);
            }
        } else if (localBinds.containsKey(name)) {
            localBinds.remove(name);
            String newName = name+"_"+objectId;
            sql = sql.replaceAll(":"+name+"\\b", ":"+newName);
            bindTo(localBinds, newName, value);
        } else {
            System.err.printf("Attempt to bind name '%s' not present in SQL: %s\n", name, sql);
        }
    }

//    protected Expr<S> bindLocal(Object value) {
//        return bindLocal("?", value);
//    }

//    protected Expr<S> bindLocal(List<Object> values) {
//        values.forEach(value -> bindLocal("?", value));
//        return this;
//    }

//    public Expr<S> bindLocal(Map<String, Object> localBinds) {
//        localBinds.forEach(this::bindLocal);
//        return this;
//    }

//    public Expr<S> bindLocal(String name, Object value) {
//        if ("?".equals(name)) {
//            bindLocal1(name, value);
//        } else {
//            if (binds.containsKey(name)) {
//                bindLocal1(name, value);
//                binds.remove(name);
//            } else if (localBinds.containsKey(name +"_"+ objectId)) {
//                bindTo(localBinds, name +"_"+ objectId, value);
//            }
//        }
//        return this;
//    }

//    private void bindLocal1(String name, Object value) {
//        if (StringHelpers.isEmpty(sql)) {
//            return;
//        }
//        if (value instanceof Expr) {
//            Expr<?> expr = (Expr) value;
//            if ("?".equals(name)) {
//                sql = sql.replaceFirst("\\?", expr.sql());
//            } else {
//                sql = sql.replaceAll(":"+ name +"\\b", expr.sql());
//            }
//            return;
//        }
//
//        final String bindName;
//        if ("?".equals(name)) {
//            bindName = "_" + ++varId + "_" + objectId;
//            sql = sql.replaceFirst("\\?", ":" + bindName);
//        } else {
//            bindName = name +"_"+ objectId;
//            sql = sql.replaceAll(":"+ name +"\\b", ":"+ bindName);
//        }
//
//        if (value instanceof UnsafeString) {
//            localBinds.put(bindName, ((UnsafeString) value).inject());
//        } else {
//            localBinds.put(bindName, value);
//        }
//    }

    public Set<Map.Entry<String, Object>> allBindEntries() {
//        Set<Map.Entry<String, Object>> set1 = binds.entrySet();
//        Set<Map.Entry<String, Object>> set2 = localBinds.entrySet();
//        return Sets.union(set1, set2);
        return localBinds().entrySet();
    }

//    public Map<String, ?> binds() {
//        return binds;
//    }

    public LinkedHashMap<String, Object> localBinds() {
        return localBinds;
    }

    private Object bindTo(Map<String, Object> binds, String name, Object value) {
        if (value instanceof UnsafeString) {
            return binds.put(name, ((UnsafeString) value).inject());
        } else {
            return binds.put(name, value);
        }
    }

    @Override
    public String toString() {
        String bind = "";
        Iterator<Map.Entry<String, Object>> bound = localBinds().entrySet().stream()
                .filter(me -> me.getValue() != null).iterator();
        String bindings = Joiner.on(", ").withKeyValueSeparator(":").join(bound);
        if (!"".equals(bindings)) {
            bind = " BIND: " + bindings;
        }
        return String.format("<SQL: %s;%s>", sql(), bind);
    }

//    public Map<String, ?> allBinds() {
//        if (!binds.isEmpty() && !localBinds.isEmpty()) {
//            SortedMap<String, Object> allBinds = new TreeMap<>(binds);
//            allBinds.putAll(localBinds);
//            return allBinds;
//        } else if (!binds.isEmpty()) {
//            return binds;
//        } else if (!localBinds.isEmpty()) {
//            return localBinds;
//        }
//        return emptyMap();
//    }

    public Object eval() {
        String sql = sql();
        Matcher matcher = GROUPED_NAME_PATTERN.matcher(sql);
        if (matcher.matches()) {
            String name = matcher.group(1);
            return localBinds().get(name);
        }
        return null;
    }

    public static String sqlTerm(Sql expr) {
        if (expr.isTerm()) {
            return expr.sql();
        }
        return "(" + expr.sql() + ")";
    }
}
