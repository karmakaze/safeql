package org.keithkim.safeql.predicate;

import com.google.common.collect.Sets;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

import java.util.Map;
import java.util.Set;

import static org.keithkim.safeql.predicate.Predicates.FALSE;

@EqualsAndHashCode(callSuper = true)
public class In<T> extends BinaryPredicate<T> {
    private final Expr<T> subject;
    private final Expr<Set<T>> range;

    public In(Expr<T> subject, Expr<Set<T>> range) {
        super(expandedSql(subject, range), null, "IN", null);
        this.subject = subject;
        this.range = range;
    }

    @Override
    public void bind(String name, Object value) {
        subject.bind(name, value);
        range.bind(name, value);
    }

    @Override
    public boolean isTerm() {
        return false;
    }

    public String sql() {
        return expandedSql(subject, range);
    }

    protected static <T> String expandedSql(Expr<T> subject, Expr<Set<T>> range) {
        if (range instanceof org.keithkim.safeql.type.Set) {
            org.keithkim.safeql.type.Set<T> sqlSet = (org.keithkim.safeql.type.Set) range;
            if (sqlSet.isKnownEmpty()) {
                return FALSE.sql();
            }
        }
        return subject.sql() +" IN "+ grouped(range.sql());
    }

    public Set<Map.Entry<String, Object>> allBindEntries() {
        return Sets.union(subject.allBindEntries(), range.allBindEntries());
    }
}
