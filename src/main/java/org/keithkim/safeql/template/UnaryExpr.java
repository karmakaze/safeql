package org.keithkim.safeql.template;

import java.util.Map;

import static org.keithkim.safeql.sql.expression.Helpers.group;

public class UnaryExpr<T> extends Expr<T> {
    private final Expr<T> expr;

    public UnaryExpr(String operator, Expr<T> expr) {
        super(operator);
        this.expr = expr;
    }

    public Expr<T> resolve(Map<String, ?> params) {
        String operator = super.string;
        if (operator.endsWith("()")) {
            return new Expr<>(operator.substring(0, operator.length() - 1) + expr.resolve(params).toString() +")");
        } else {
            return new Expr<>(operator + " " + group(expr.resolve(params).toString()));
        }
    }
}
