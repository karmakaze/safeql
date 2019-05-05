package org.keithkim.safeql.sql.expression;

import com.google.common.base.Joiner;
import org.keithkim.safeql.template.Expr;

import java.util.Map;

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

    public Expr<T> resolve(Map<String, ?> params) {
        return Expr.expr(Joiner.on(" " + type.toString() + " ").join(tables));
    }

//    public void bind(String name, Object value) {
//        params().put(name, Optional.of(value));
//        for (SqlTable<?> table : tables) {
//            table.bind(name, value);
//        }
//    }

//    public Map<String, Optional<?>> params() {
//        Map<String, Optional<?>> params = new HashMap<>();
//        for (SelectQuery q : selectQueries) {
//            params.putAll(q.params());
//        }
//        return params;
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
