package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.Expr;

import java.util.Collection;

public class Predicates {
    public static final Predicate FALSE = new Predicate("FALSE", true) {};
    public static final Predicate TRUE = new Predicate("TRUE", true ) {};

    private final static BooleanPredicateFactory ALL_OF = BooleanPredicateFactory.newOperation("AND", Predicates.TRUE,
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

    private final static BooleanPredicateFactory ANY_OF = BooleanPredicateFactory.newOperation("OR", Predicates.FALSE,
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

    public static Predicate ALL(Collection<? extends Predicate> predicates) {
        return ALL_OF.newPredicate(predicates);
    }

    public static Predicate OR(Predicate left, Predicate right) {
        return ANY_OF.newPredicate(left, right);
    }

    public static Predicate ANY(Predicate... predicates) {
        return ANY_OF.newPredicate(predicates);
    }

    public static Predicate ANY(Collection<? extends Predicate> predicates) {
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

    public static <T> Predicate IN(Expr<T> left, Expr<T> right) {
        return new In(left, right);
    }

    public static Predicate LIKE(Expr<String> subject, Expr<String> pattern) {
        return new Like(subject, pattern);
    }

    public static Predicate ILIKE(Expr<String> subject, Expr<String> pattern) {
        return new ILike(subject, pattern);
    }
}
