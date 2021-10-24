package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;

import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

@EqualsAndHashCode(callSuper = true)
public class Not extends Predicate {
    private final Predicate predicate;

    public Not(Predicate predicate) {
        super("NOT");
        this.predicate = predicate;
    }

    public String sql() {
        if (predicate.isKnownFalse()) {
            return TRUE.sql();
        }
        if (predicate.isKnownTrue()) {
            return FALSE.sql();
        }
        return "NOT " + group(predicate.sql());
    }
}
