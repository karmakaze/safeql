package org.keithkim.safeql.predicate;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;

import java.util.Collection;
import java.util.List;

import static java.util.stream.Collectors.toList;

@EqualsAndHashCode(callSuper = true)
public class BooleanPredicate extends Predicate {
    private final Predicate identity;
    private final Collection<? extends Predicate> predicates;

    public BooleanPredicate(String operator, Predicate identity, Collection<? extends Predicate> predicates) {
        super(operator);
        this.identity = identity;
        this.predicates = predicates;
    }

    protected Collection<? extends Predicate> predicates() {
        return predicates;
    }

    public String sql() {
        if (predicates.isEmpty()) {
            return identity.sql();
        } else if (predicates.size() == 1) {
            return predicates.iterator().next().sql();
        }

        String operator = super.sql();
        List<String> terms = predicates.stream().map(p -> group(p.sql())).collect(toList());

        if (operator.endsWith("()")) {
            String arguments = Joiner.on(", ").join(terms);
            return operator.substring(0, operator.length() - 1) + arguments +")";
        } else {
            return Joiner.on(" "+operator+" ").join(terms);
        }
    }
}
