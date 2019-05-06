package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

@EqualsAndHashCode
public class Equal<T> extends BinaryPredicate<T> {
    public Equal(Expr<T> left, Expr<T> right) {
        super(left, "=", right);
    }

    public String sql() {
        String leftSql = left().sql();
        String rightSql = right().sql();
        if ("NULL".equalsIgnoreCase(leftSql)) {
            if ("NULL".equalsIgnoreCase(rightSql)) {
                // TODO do we ever want this to mean TRUE?
                return "FALSE";
            } else {
                return group(rightSql) + " IS NULL";
            }
        } else {
            if ("NULL".equalsIgnoreCase(rightSql)) {
                return group(leftSql) + " IS NULL";
            } else {
                return group(leftSql) + " = " + group(rightSql);
            }
        }
    }
}
