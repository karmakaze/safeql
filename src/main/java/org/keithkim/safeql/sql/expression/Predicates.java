package org.keithkim.safeql.sql.expression;

import static org.keithkim.safeql.sql.expression.Helpers.group;

public class Predicates {
    public static final Predicate FALSE = new Predicate("FALSE");
    public static final Predicate TRUE = new Predicate("TRUE");

    public static Predicate Not(Predicate pred) {
        return new Not(pred);
    }

    public static Predicate And(Predicate left, Predicate right) {
        return new And(left, right);
    }

    public static Predicate Or(Predicate left, Predicate right) {
        return new Or(left, right);
    }

    public static class Not extends Predicate {
        private final Predicate pred;

        public Not(Predicate pred) {
            super("NOT");
            this.pred = pred;
        }

        public String sql() {
            if (pred.isKnownFalse()) {
                return TRUE.sql();
            }
            if (pred.isKnownTrue()) {
                return FALSE.sql();
            }
            return "NOT " + group(pred.sql());
        }
    }

    public static class And extends BinaryPred<Boolean> {
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

    public static class Or extends BinaryPred<Boolean> {
        public Or(Predicate left, Predicate right) {
            super(left, "AND", right);
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
}
