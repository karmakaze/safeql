package org.keithkim.safeql.sql;

import org.keithkim.safeql.template.Expr;

import java.util.Map;

import static org.keithkim.safeql.template.Helpers.group;

public class SqlEqual<T> {
    private final Expr<T> left;
    private final Expr<T> right;

    public SqlEqual(Expr<T> left, Expr<T> right) {
        this.left = left;
        this.right = right;
    }

    public Expr<Boolean> render(Map<String, ?> params) {
        String leftString = left.render(params).toString();
        String rightString = right.render(params).toString();
        if ("NULL".equalsIgnoreCase(leftString)) {
            if ("NULL".equalsIgnoreCase(rightString)) {
                // TODO do we ever want this to mean TRUE?
                return new Expr<>("FALSE");
            } else {
                return new Expr<>(group(rightString) + " IS NULL");
            }
        } else {
            if ("NULL".equalsIgnoreCase(rightString)) {
                return new Expr<>(group(leftString) + " IS NULL");
            } else {
                return new Expr<>(group(leftString) + " = " + group(rightString));
            }
        }
    }
}
