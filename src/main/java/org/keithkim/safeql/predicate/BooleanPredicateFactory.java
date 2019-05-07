package org.keithkim.safeql.predicate;

import java.util.Collection;
import java.util.function.BiFunction;

import static java.util.Arrays.asList;

public class BooleanPredicateFactory {
    private final String operator;
    private final Predicate identity;
    private final BiFunction<String, Collection<? extends Predicate>, String> sqlFn;

    public static BooleanPredicateFactory newOperation(String operator, Predicate identity,
                                                       BiFunction<String, Collection<? extends Predicate>, String> sqlFn) {
        return new BooleanPredicateFactory(operator, identity, sqlFn);
    }

    public BooleanPredicateFactory(String operator, Predicate identity,
                                   BiFunction<String, Collection<? extends Predicate>, String> sqlFn) {
        this.operator = operator;
        this.identity = identity;
        this.sqlFn = sqlFn;
    }

    public BooleanPredicate newPredicate(Predicate... predicates) {
        return newPredicate(asList(predicates));
    }

    public BooleanPredicate newPredicate(Collection<? extends Predicate> predicates) {
        return new BooleanPredicate(operator, identity, predicates) {
            public String sql() {
                return sqlFn.apply(super.sql(), predicates);
            }
        };
    }
}
