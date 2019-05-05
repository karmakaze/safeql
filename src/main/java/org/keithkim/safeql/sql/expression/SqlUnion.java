package org.keithkim.safeql.sql.expression;

import com.google.common.base.Joiner;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

public class SqlUnion<T> extends Expr<T> {
    private final Type type;
    private final SqlTable[] tables;

    public SqlUnion(SqlTable<?>... tables) {
        this(Type.UNION, tables);
    }

    public SqlUnion(Type type, SqlTable<?>... tables) {
        super(null);
        this.type = type;
        this.tables = tables;
    }

    public String sql() {
        return Joiner.on(" " + type.toString() + " ").join(asList(tables).stream().map(SqlTable::sql).collect(toList()));
    }

//    public void bind(String name, Object value) {
//        bindings().put(name, Optional.of(value));
//        for (SqlTable<?> table : tables) {
//            table.bind(name, value);
//        }
//    }

//    public Map<String, Optional<?>> bindings() {
//        Map<String, Optional<?>> bindings = new HashMap<>();
//        for (SelectQuery q : selectQueries) {
//            bindings.putAll(q.bindings());
//        }
//        return bindings;
//    }

    public enum Type {
        UNION("UNION"),
        UNION_DISTINCT("UNION DISTINCT"),
        UNION_ALL("UNION ALL");

        private final String symbol;

        Type(String symbol) {
            this.symbol = symbol;
        }

        public String toString() {
            return symbol;
        }
    }
}
