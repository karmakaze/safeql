package org.keithkim.safeql.template;

import java.util.Map;

import static org.keithkim.safeql.template.Helpers.group;

public class UnaryExpr<T> extends Expr<T> {
    private final Expr<T> expr;

    public UnaryExpr(String operator, Expr<T> expr) {
        super(operator);
        this.expr = expr;
    }

    Expr<T> render(Map<String, ?> params) {
        String operator = super.string;
        if (operator.endsWith("()")) {
            return new Expr<>(operator.substring(0, operator.length() - 1) + expr.render(params).toString() +")");
        } else {
            return new Expr<>(operator + " " + group(expr.render(params).toString()));
        }
    }
}
