package org.keithkim.typeql.template;

import java.util.Map;

import static org.keithkim.typeql.template.Helpers.group;

public class TernaryExpr<T> extends Expr<T> {
    private final Expr<T> expr1;
    private final String operator1;
    private final Expr<T> expr2;
    private final String operator2;
    private final Expr<T> expr3;

    public TernaryExpr(Expr<T> expr1, String operator1, Expr<T> expr2, String operator2, Expr<T> expr3) {
        super(null);
        this.expr1 = expr1;
        this.operator1 = operator1;
        this.expr2 = expr2;
        this.operator2 = operator2;
        this.expr3 = expr3;
    }

    Expr<T> render(Map<String, ?> params) {
        String string = group(expr1.render(params).toString()) +
                " "+ operator1 + " " +
                group(expr2.render(params).toString()) +
                " "+ operator2 + " " +
                group(expr3.render(params).toString());
        return new Expr<>(string);
    }
}
