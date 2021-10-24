package org.keithkim.safeql.predicate;

public abstract class BooleanBinaryPredicate extends BinaryPredicate<Boolean> {
    public BooleanBinaryPredicate(Predicate left, String operator, Predicate right) {
        super(left, operator, right);
    }

    protected Predicate left() {
        return (Predicate) super.left();
    }

    protected Predicate right() {
        return (Predicate) super.right();
    }
}
