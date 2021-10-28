package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.TernaryExpr;

@EqualsAndHashCode(callSuper = false)
public class Between<T> extends TernaryExpr<Boolean, T, T, T> implements Predicate {
    public Between(Expr<T> subject, Expr<T> rangeMin, Expr<T> rangeMax) {
        super(subject, "BETWEEN", rangeMin, "AND", rangeMax);
    }
}
