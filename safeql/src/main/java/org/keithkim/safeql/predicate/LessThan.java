package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

@EqualsAndHashCode(callSuper = true)
public class LessThan<T> extends BinaryPredicate<T, T> {
    public LessThan(Expr<T> left, Expr<T> right) {
        super(left, "<", right);
    }

//    public String sql() {
//        String leftSql = left().sql();
//        String rightSql = right().sql();
//        if ("NULL".equalsIgnoreCase(leftSql) || "NULL".equalsIgnoreCase(rightSql)) {
//            return "NULL";
//        } else {
//            return group(leftSql) + " < " + group(rightSql);
//        }
//    }
}
