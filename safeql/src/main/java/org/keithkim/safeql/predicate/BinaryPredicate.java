package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.BinaryExpr;
import org.keithkim.safeql.expression.Expr;

import java.util.Objects;

public abstract class BinaryPredicate<T> extends Predicate {
    private final BinaryExpr<T> binaryExpr;

    public BinaryPredicate(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.binaryExpr = new BinaryExpr<T>(left, operator, right) {};
    }

    protected Expr<T> left() {
        return binaryExpr.component(0);
    }

    protected Expr<T> right() {
        return binaryExpr.component(1);
    }

    public String sql() {
        return binaryExpr.sql();
    }

    public boolean equals(Object other) {
        return Objects.equals(binaryExpr, other);
    }

    public int hashCode() {
        return Objects.hashCode(binaryExpr);
    }
}
