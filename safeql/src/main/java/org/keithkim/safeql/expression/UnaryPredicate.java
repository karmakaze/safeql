package org.keithkim.safeql.expression;

import org.keithkim.safeql.predicate.Predicate;

import java.util.Map;
import java.util.Set;

import static java.util.Collections.emptySet;

public abstract class UnaryPredicate<T> extends Expr<Boolean> implements Predicate {
    public UnaryPredicate(String operator, SqlScalar<T> expr) {
        this(null, operator, expr, emptySet());
    }

    public UnaryPredicate(String sql, String operator, SqlScalar<T> expr) {
        this(expandSql(sql, operator, expr), operator, expr, emptySet());
    }

    public UnaryPredicate(String operator, SqlScalar<T> expr, Set<Map.Entry<String, Object>> bindEntries) {
        this(null, operator, expr, bindEntries);
    }

    public UnaryPredicate(String sql, String operator, SqlScalar<T> expr, Set<Map.Entry<String, Object>> bindEntries) {
        super(expandSql(sql, operator, expr), true, bindEntries, true);
    }

    protected static <T> String expandSql(String sql, String operator, SqlScalar<T> expr) {
        if (sql != null && !sql.isEmpty()) {
            return sql;
        }
//        if (expr instanceof Predicate) {
//            Predicate predicate = (Predicate) expr;
//            if (predicate.isKnownFalse()) {
//                return Predicates.TRUE.sql();
//            } else if (predicate.isKnownTrue()) {
//                return Predicates.FALSE.sql();
//            }
//        }
        if (operator.endsWith("()")) {
            return operator.substring(0, operator.length()-1) + expr.sql() +")";
        } else {
            return operator +" "+ sqlTerm(expr);
        }
    }

//    public String sql() {
//        String operator = super.sql();
//
//        if (operator.endsWith("()")) {
//            return operator.substring(0, operator.length() - 1) + expr.sql() +")";
//        } else {
//            return operator + " " + group(expr.sql());
//        }
//    }
}
