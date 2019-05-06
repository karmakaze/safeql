package org.keithkim.safeql.expression;

import lombok.EqualsAndHashCode;

@EqualsAndHashCode
public class UnaryExpr<T> extends Expr<T> {
    private final Expr<T> expr;

    public UnaryExpr(String operator, Expr<T> expr) {
        super(operator);
        this.expr = expr;
    }

    public String sql() {
        String operator = super.sql();

        if (operator.endsWith("()")) {
            return operator.substring(0, operator.length() - 1) + expr.sql() +")";
        } else {
            return operator + " " + group(expr.sql());
        }
    }
}
