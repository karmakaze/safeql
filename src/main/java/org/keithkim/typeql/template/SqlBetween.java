package org.keithkim.typeql.template;

public class SqlBetween<T> extends TernaryExpr<T> {
    public SqlBetween(Expr<T> subject, Expr<T> rangeMin, Expr<T> rangeMax) {
        super(subject, "BETWEEN", rangeMin, "AND", rangeMax);
    }
}
