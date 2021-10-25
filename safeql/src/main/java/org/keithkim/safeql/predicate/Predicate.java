package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.Expr;

public abstract class Predicate extends Expr<Boolean> {
    public Predicate(String sql) {
        super(sql);
    }

    protected Predicate(String sql, boolean isTerm) {
        super(sql, isTerm);
    }

    public boolean isKnownFalse() {
        return "FALSE".equalsIgnoreCase(sql()) || Boolean.FALSE == eval();
    }

    public boolean isKnownTrue() {
        return "TRUE".equalsIgnoreCase(sql()) || Boolean.TRUE == eval();
    }

}
