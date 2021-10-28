package org.keithkim.safeql.predicate;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.Collections.emptySet;

@EqualsAndHashCode(callSuper = true)
public abstract class NAryPredicate extends Expr<Boolean> implements Predicate {
    private final String operator;
    private final Predicate identity;
    private final List<Predicate> predicates;

    public NAryPredicate(String operator, Predicate identity, Predicate... predicates) {
        this(operator, identity, asList(predicates));
    }

    public NAryPredicate(String operator, Predicate identity, List<Predicate> predicates) {
        this(operator, identity, predicates, emptySet());
    }

    public NAryPredicate(String operator, Predicate identity, List<Predicate> predicates, Set<Map.Entry<String, Object>> bindEntries) {
        super(expandSql(operator, identity, predicates), expandIsTerm(identity, predicates), bindEntries, true);
        this.operator = operator;
        this.identity = identity;
        this.predicates = predicates;
    }

    protected List<Predicate> predicates() {
        return predicates;
    }

    protected static boolean expandIsTerm(Predicate identity, List<Predicate> predicates) {
        if (predicates.isEmpty()) {
            return identity.isTerm();
        } else if (predicates.size() == 1) {
            return predicates.get(0).isTerm();
        }
        return false;
    }

    protected static String expandSql(String operator, Predicate identity, List<Predicate> predicates) {
        if (predicates.isEmpty()) {
            return identity.sql();
        } else if (predicates.size() == 1) {
            return predicates.get(0).sql();
        }

        Stream<String> terms = predicates.stream().map(Expr::sqlTerm);
        if (operator.endsWith("()")) {
            String arguments = Joiner.on(", ").join(terms.iterator());
            return operator.substring(0, operator.length() - 1) + arguments +")";
        } else {
            return Joiner.on(" "+operator+" ").join(terms.iterator());
        }
    }
}
