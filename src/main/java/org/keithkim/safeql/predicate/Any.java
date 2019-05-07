package org.keithkim.safeql.predicate;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;

import java.util.Collection;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

@EqualsAndHashCode
public class Any extends Predicate {
    private final Collection<? extends Predicate> predicates;

    public Any(Predicate... predicates) {
        super("OR");
        this.predicates = asList(predicates);
    }

    public Any(Collection<? extends Predicate> predicates) {
        super("OR");
        this.predicates = predicates;
    }

    public String sql() {
        boolean isKnownFalse = true;
        for (Predicate predicate : predicates) {
            if (predicate.isKnownTrue()) {
                return Predicates.TRUE.sql();
            }
            if (!predicate.isKnownFalse()) {
                isKnownFalse = false;
            }
        }
        if (isKnownFalse) {
            return Predicates.FALSE.sql();
        }
        return Joiner.on(" OR ").join(predicates.stream().map(e -> group(e)).collect(toList()));
    }
}
