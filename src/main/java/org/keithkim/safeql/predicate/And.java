package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;

@EqualsAndHashCode
public class And extends BinaryPredicate<Boolean> {
    public And(Predicate left, Predicate right) {
        super(left, "AND", right);
    }

    public String sql() {
        Predicate left = (Predicate) left();
        Predicate right = (Predicate) right();
        if (left.isKnownFalse() || right.isKnownFalse()) {
            return Predicates.FALSE.sql();
        }
        if (left.isKnownTrue() && right.isKnownTrue()) {
            return Predicates.TRUE.sql();
        }
        return super.sql();
    }
}
