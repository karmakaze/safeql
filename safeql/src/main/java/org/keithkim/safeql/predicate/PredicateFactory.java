package org.keithkim.safeql.predicate;

import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;

import static java.util.Arrays.asList;

public class PredicateFactory {
    private final String operator;
    private final Predicate identity;
    private final BiFunction<String, Collection<? extends Predicate>, String> sqlFn;

    public static PredicateFactory newOperation(String operator, Predicate identity,
                                                BiFunction<String, Collection<? extends Predicate>, String> sqlFn) {
        return new PredicateFactory(operator, identity, sqlFn);
    }

    public PredicateFactory(String operator, Predicate identity,
                            BiFunction<String, Collection<? extends Predicate>, String> sqlFn) {
        this.operator = operator;
        this.identity = identity;
        this.sqlFn = sqlFn;
    }

    public Predicate newPredicate(Predicate... predicates) {
        return newPredicate(asList(predicates));
    }

    public Predicate newPredicate(List<Predicate> predicates) {
        return new NAryPredicate(operator, identity, predicates) {
            public String sql() {
                return sqlFn.apply(super.sql(), predicates);
            }
        };
    }
}
