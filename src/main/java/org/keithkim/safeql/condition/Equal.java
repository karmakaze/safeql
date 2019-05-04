package org.keithkim.safeql.condition;

import org.keithkim.safeql.template.Expr;

import java.util.Map;

import static org.keithkim.safeql.sql.Helpers.group;

public class Equal<T> extends Expr<Boolean> {
    public final Expr<T> left;
    public final Expr<T> right;

    public Equal(Expr<T> left, Expr<T> right) {
        super(null);
        this.left = left;
        this.right = right;
    }

    public Expr<Boolean> resolve(Map<String, ?> params) {
        return Expr.expr(group(left.toString()) +" = "+ group(right.toString()));
    }
}
