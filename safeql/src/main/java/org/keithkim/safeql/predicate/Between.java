package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.TernaryExpr;

@EqualsAndHashCode(callSuper = false)
public class Between<T> extends Predicate {
    private final TernaryExpr<T> ternaryExpr;

    public Between(Expr<T> subject, Expr<T> rangeMin, Expr<T> rangeMax) {
        super(null);
        this.ternaryExpr = new TernaryExpr<T>(subject, "BETWEEN", rangeMin, "AND", rangeMax) {};
    }

    @Override
    public void bind(String name, Object value) {
        ternaryExpr.bind(name, value);
    }

    @Override
    public String sql() {
        return ternaryExpr.sql();
    }

    @Override
    public String toString() {
        return ternaryExpr.toString();
    }
}
