package org.keithkim.safeql.predicate;

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

}
