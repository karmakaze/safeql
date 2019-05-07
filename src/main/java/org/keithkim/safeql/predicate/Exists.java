package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;

@EqualsAndHashCode
public class Exists extends Predicate {
    private final Predicate predicate;

    public Exists(Predicate predicate) {
        super("NOT");
        this.predicate = predicate;
    }

    public String sql() {
        if (predicate.isKnownFalse()) {
            return Predicates.TRUE.sql();
        }
        if (predicate.isKnownTrue()) {
            return Predicates.FALSE.sql();
        }
        return "NOT " + group(predicate.sql());
    }
}
