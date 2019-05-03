package org.keithkim.safeql.template;

import java.util.Map;

public class BinaryExpr<T> extends Expr<T> {
    private final NAryExpr<T> nAryExpr;

    public BinaryExpr(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.nAryExpr = new NAryExpr<>(operator, null, left, right);
    }

    public Expr<T> resolve(Map<String, ?> params) {
        return nAryExpr.resolve(params);
    }
}
