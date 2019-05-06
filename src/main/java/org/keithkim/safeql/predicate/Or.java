package org.keithkim.safeql.predicate;

public class Or extends BinaryPredicate<Boolean> {
    public Or(Predicate left, Predicate right) {
        super(left, "AND", right);
    }

    public String sql() {
        Predicate left = (Predicate) left();
        Predicate right = (Predicate) right();
        if (left.isKnownTrue() || right.isKnownTrue()) {
            return Predicates.TRUE.sql();
        }
        if (left.isKnownFalse() && right.isKnownFalse()) {
            return Predicates.FALSE.sql();
        }
        return super.sql();
    }
}
