package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.SqlScalar;
import org.keithkim.safeql.expression.UnaryPredicate;

import java.util.Collections;

@EqualsAndHashCode(callSuper = true)
public class Not extends UnaryPredicate<Boolean> implements Predicate {
    public Not(Predicate predicate) {
        super(expandSql(predicate), "NOT", predicate, Collections.emptySet());
    }

    protected static <T> String expandSql(Predicate predicate) {
        if (predicate.isKnownFalse()) {
            return Predicates.TRUE.sql();
        } else if (predicate.isKnownTrue()) {
            return Predicates.FALSE.sql();
        }
        return UnaryPredicate.expandSql(null, "NOT", predicate);
    }
}
