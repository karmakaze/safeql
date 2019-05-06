package org.keithkim.safeql.expression;

import lombok.EqualsAndHashCode;

@EqualsAndHashCode
public class BinaryExpr<T> extends Expr<T> {
    private final NAryExpr<T> nAryExpr;

    public BinaryExpr(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.nAryExpr = new NAryExpr<>(operator, null, left, right);
    }

    public Expr<T> component(int i) {
        return nAryExpr.component(i);
    }

    public String sql() {
        return nAryExpr.sql();
    }
}
