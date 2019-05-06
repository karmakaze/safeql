package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.BinaryExpr;
import org.keithkim.safeql.expression.Expr;

public class Predicate extends Expr<Boolean> {
    public Predicate(String sql) {
        super(sql);
    }

    public boolean isKnownFalse() {
        return "FALSE".equalsIgnoreCase(sql()) || Boolean.FALSE == eval();
    }

    public boolean isKnownTrue() {
        return "TRUE".equalsIgnoreCase(sql()) || Boolean.TRUE == eval();
    }

}