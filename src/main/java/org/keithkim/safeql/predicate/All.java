package org.keithkim.safeql.predicate;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;

import java.util.Collection;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

@EqualsAndHashCode
public class All extends Predicate {
    private final Collection<? extends Predicate> predicates;

    public All(Predicate... predicates) {
        super("AND");
        this.predicates = asList(predicates);
    }

    public All(Collection<? extends Predicate> predicates) {
        super("AND");
        this.predicates = predicates;
    }

    public String sql() {
        boolean isKnownTrue = true;
        for (Predicate predicate : predicates) {
            if (predicate.isKnownFalse()) {
                return Predicates.FALSE.sql();
            }
            if (!predicate.isKnownTrue()) {
                isKnownTrue = false;
            }
        }
        if (isKnownTrue) {
            return Predicates.TRUE.sql();
        }
        return Joiner.on(" AND ").join(predicates.stream().map(e -> group(e)).collect(toList()));
    }
}
