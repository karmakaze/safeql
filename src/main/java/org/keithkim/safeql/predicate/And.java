package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;

import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

@EqualsAndHashCode(callSuper = true)
public class And extends BinaryPredicate<Boolean> {
    public And(Predicate left, Predicate right) {
        super(left, "AND", right);
    }

    public String sql() {
        Predicate left = (Predicate) left();
        Predicate right = (Predicate) right();
        if (left.isKnownFalse() || right.isKnownFalse()) {
            return FALSE.sql();
        }
        if (left.isKnownTrue() && right.isKnownTrue()) {
            return TRUE.sql();
        }
        return super.sql();
    }
}
