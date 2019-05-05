package org.keithkim.safeql.sql.expression;

public class SqlBetween<T> extends TernaryExpr<T> {
    public SqlBetween(Expr<T> subject, Expr<T> rangeMin, Expr<T> rangeMax) {
        super(subject, "BETWEEN", rangeMin, "AND", rangeMax);
    }
}
