package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;

import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

@EqualsAndHashCode(callSuper = true)
public class Or extends BinaryPredicate<Boolean> {
    public Or(Predicate left, Predicate right) {
        super(left, "OR", right);
    }

    public String sql() {
        Predicate left = (Predicate) left();
        Predicate right = (Predicate) right();
        if (left.isKnownTrue() || right.isKnownTrue()) {
            return TRUE.sql();
        }
        if (left.isKnownFalse() && right.isKnownFalse()) {
            return FALSE.sql();
        }
        return super.sql();
    }
}
