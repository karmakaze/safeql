package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.*;

import java.util.*;

import static java.util.Collections.emptySet;

public class Predicates {
    private static class LiteralPredicate extends LiteralExpr<Boolean> implements Predicate {
        final boolean value;
        public LiteralPredicate(String sqlLiteral, boolean value) {
            super(sqlLiteral, emptySet());
            this.value = value;
        }
        @Override
        public boolean isKnownTrue() {
            return value;
        }
        @Override
        public boolean isKnownFalse() {
            return !value;
        }
    }

    public static final Predicate FALSE = new LiteralPredicate("FALSE", false);
    public static final Predicate TRUE = new LiteralPredicate("TRUE", true );

    private final static PredicateFactory ALL_OF = PredicateFactory.newOperation("AND", Predicates.TRUE,
            (sql, predicates) -> {
                boolean isKnownTrue = true;
                for (Predicate predicate : predicates) {
                    if (predicate.isKnownFalse()) {
                        return FALSE.sql();
                    }
                    if (!predicate.isKnownTrue()) {
                        isKnownTrue = false;
                    }
                }
                if (isKnownTrue) {
                    return TRUE.sql();
                }
                return sql;
            });

    private final static PredicateFactory ANY_OF = PredicateFactory.newOperation("OR", Predicates.FALSE,
            (sql, predicates) -> {
                boolean isKnownFalse = true;
                for (Predicate predicate : predicates) {
                    if (predicate.isKnownTrue()) {
                        return TRUE.sql();
                    }
                    if (!predicate.isKnownFalse()) {
                        isKnownFalse = false;
                    }
                }
                if (isKnownFalse) {
                    return FALSE.sql();
                }
                return sql;
            });

    public static Predicate NOT(Predicate predicate) {
        return new Not(predicate);
    }

    public static Predicate AND(Predicate left, Predicate right) {
        return ALL_OF.newPredicate(left, right);
    }

    public static Predicate ALL(Predicate... predicates) {
        return ALL_OF.newPredicate(predicates);
    }

    public static Predicate ALL(List<Predicate> predicates) {
        return ALL_OF.newPredicate(predicates);
    }

    public static Predicate OR(Predicate left, Predicate right) {
        return ANY_OF.newPredicate(left, right);
    }

    public static Predicate ANY(Predicate... predicates) {
        return ANY_OF.newPredicate(predicates);
    }

    public static Predicate ANY(List<Predicate> predicates) {
        return ANY_OF.newPredicate(predicates);
    }

    public static <T> Predicate EQ(Expr<T> left, Expr<T> right) {
        return new Equal<>(left, right);
    }

    public static <T> Predicate NE(Expr<T> left, Expr<T> right) {
        return new NotEqual<>(left, right);
    }

    public static <T> Predicate LT(Expr<T> left, Expr<T> right) {
        return new LessThan<>(left, right);
    }

    public static <T> Predicate LTE(Expr<T> left, Expr<T> right) {
        return new LessThanOrEqual<>(left, right);
    }

    public static <T> Predicate GT(Expr<T> left, Expr<T> right) {
        return new GreaterThan<>(left, right);
    }

    public static <T> Predicate GTE(Expr<T> left, Expr<T> right) {
        return new GreaterThanOrEqual<>(left, right);
    }

    public static <T> Predicate IN(Sql<T> left, SqlSet<T> right) {
        return new In<>(left, right);
    }

    public static Predicate LIKE(Expr<String> subject, Expr<String> pattern) {
        return new Like(subject, pattern);
    }

    public static Predicate ILIKE(Expr<String> subject, Expr<String> pattern) {
        return new ILike(subject, pattern);
    }
}
