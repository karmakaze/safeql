package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.Expr;

import java.util.Collection;

public class Predicates {
    public static final Predicate FALSE = new Predicate("FALSE") {};
    public static final Predicate TRUE = new Predicate("TRUE") {};

    public static Predicate NOT(Predicate predicate) {
        return new Not(predicate);
    }

    public static Predicate AND(Predicate left, Predicate right) {
        return new And(left, right);
    }

    public static Predicate ALL(Predicate... predicates) {
        return new All(predicates);
    }

    public static Predicate ALL(Collection<? extends Predicate> predicates) {
        return new All(predicates);
    }

    public static Predicate OR(Predicate left, Predicate right) {
        return new Or(left, right);
    }

    public static Predicate ANY(Predicate... predicates) {
        return new Any(predicates);
    }

    public static Predicate ANY(Collection<? extends Predicate> predicates) {
        return new Any(predicates);
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

    public static Predicate LIKE(Expr<String> subject, Expr<String> pattern) {
        return new Like(subject, pattern);
    }

    public static Predicate ILIKE(Expr<String> subject, Expr<String> pattern) {
        return new ILike(subject, pattern);
    }
}
