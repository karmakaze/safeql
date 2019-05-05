package org.keithkim.safeql.sql.expression;

public class BinaryPred<T> extends Predicate {
    private final BinaryExpr<T> binaryExpr;

    public BinaryPred(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.binaryExpr = new BinaryExpr<>(left, operator, right);
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
}
