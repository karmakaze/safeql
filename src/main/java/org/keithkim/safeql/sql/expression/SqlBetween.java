package org.keithkim.safeql.sql.expression;

import org.keithkim.safeql.template.Expr;
import org.keithkim.safeql.template.TernaryExpr;

public class SqlBetween<T> extends TernaryExpr<T> {
    public SqlBetween(Expr<T> subject, Expr<T> rangeMin, Expr<T> rangeMax) {
        super(subject, "BETWEEN", rangeMin, "AND", rangeMax);
    }
}
