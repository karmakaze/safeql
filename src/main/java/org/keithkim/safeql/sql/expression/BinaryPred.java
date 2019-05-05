package org.keithkim.safeql.sql.expression;

public class BinaryPred<T> extends Predicate {
    private final BinaryExpr<T> binaryExpr;

    public BinaryPred(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.binaryExpr = new BinaryExpr<>(left, operator, right);
    }

    public String sql() {
        return binaryExpr.sql();
    }
}
