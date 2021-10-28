package org.keithkim.safeql.expression;

public abstract class TernaryExpr<S, L, M, R> extends Expr<S> {
    private final Expr<L> left;
    private final String operator1;
    private final Expr<M> middle;
    private final String operator2;
    private final Expr<R> right;

    public TernaryExpr(Expr<L> left, String operator1, Expr<M> middle, String operator2, Expr<R> right) {
        super(null);
        this.left = left;
        this.operator1 = operator1;
        this.middle = middle;
        this.operator2 = operator2;
        this.right = right;
    }

    public String sql() {
        return sqlTerm(left) + " "+ operator1 + " " + sqlTerm(middle) + " "+ operator2 + " " + sqlTerm(right);
    }
}
