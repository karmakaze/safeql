package org.keithkim.safeql.sql.expression;

import static org.keithkim.safeql.sql.expression.Helpers.group;

public class Equal<T> extends Expr<Boolean> {
    public final Expr<T> left;
    public final Expr<T> right;

    public Equal(Expr<T> left, Expr<T> right) {
        super(null);
        this.left = left;
        this.right = right;
    }

    public String sql() {
        return group(left.toString()) +" = "+ group(right.toString());
    }
}
