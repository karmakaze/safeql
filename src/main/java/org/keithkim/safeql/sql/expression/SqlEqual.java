package org.keithkim.safeql.sql.expression;

import static org.keithkim.safeql.sql.expression.Helpers.group;

public class SqlEqual<T> {
    private final Expr<T> left;
    private final Expr<T> right;

    public SqlEqual(Expr<T> left, Expr<T> right) {
        this.left = left;
        this.right = right;
    }

    public String sql() {
        String leftString = left.sql();
        String rightString = right.sql();
        if ("NULL".equalsIgnoreCase(leftString)) {
            if ("NULL".equalsIgnoreCase(rightString)) {
                // TODO do we ever want this to mean TRUE?
                return "FALSE";
            } else {
                return group(rightString) + " IS NULL";
            }
        } else {
            if ("NULL".equalsIgnoreCase(rightString)) {
                return group(leftString) + " IS NULL";
            } else {
                return group(leftString) + " = " + group(rightString);
            }
        }
    }
}
