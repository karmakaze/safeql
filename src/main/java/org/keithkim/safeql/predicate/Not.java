package org.keithkim.safeql.predicate;

public class Not extends Predicate {
    private final Predicate pred;

    public Not(Predicate pred) {
        super("NOT");
        this.pred = pred;
    }

    public String sql() {
        if (pred.isKnownFalse()) {
            return Predicates.TRUE.sql();
        }
        if (pred.isKnownTrue()) {
            return Predicates.FALSE.sql();
        }
        return "NOT " + group(pred.sql());
    }
}
