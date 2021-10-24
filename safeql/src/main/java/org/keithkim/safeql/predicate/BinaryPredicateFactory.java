package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.Expr;

import java.util.function.Function;

public class BinaryPredicateFactory<T> {
    private final String operator;
    private final Function<Expr<T>, String> sqlFn;

    public static <T> BinaryPredicateFactory<T> newPredicate(String operator, Function<Expr<T>, String> sqlFn) {
        return new BinaryPredicateFactory<>(operator, sqlFn);
    }

    public BinaryPredicateFactory(String operator, Function<Expr<T>, String> sqlFn) {
        this.operator = operator;
        this.sqlFn = sqlFn;
    }

    public BinaryPredicate<T> predicate(Expr<T> left, Expr<T> right) {
        return new BinaryPredicate<T>(left, operator, right) {
        };
    }
}
