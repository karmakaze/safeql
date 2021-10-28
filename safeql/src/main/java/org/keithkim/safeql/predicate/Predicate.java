package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.Eval;
import org.keithkim.safeql.expression.SqlScalar;

public interface Predicate extends Eval, SqlScalar<Boolean> {
    default boolean isKnownFalse() {
        return "FALSE".equalsIgnoreCase(sql()) || Boolean.FALSE == eval();
    }

    default boolean isKnownTrue() {
        return "TRUE".equalsIgnoreCase(sql()) || Boolean.TRUE == eval();
    }
}
