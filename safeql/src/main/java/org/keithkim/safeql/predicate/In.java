package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

import java.util.Set;

import static org.keithkim.safeql.predicate.Predicates.FALSE;

@EqualsAndHashCode(callSuper = true)
public class In<T> extends BinaryPredicate<T> {
    private final Expr<T> subject;
    private final Expr<Set> range;

    public In(Expr<T> subject, Expr<Set> range) {
        super(null, "IN", null);
        this.subject = subject;
        this.range = range;
    }

    @Override
    public void bind(String name, Object value) {
        subject.bind(name, value);
        range.bind(name, value);
    }

    public String sql() {
        if (range instanceof org.keithkim.safeql.type.Set) {
            org.keithkim.safeql.type.Set sqlSet = (org.keithkim.safeql.type.Set) range;
            if (sqlSet.isKnownEmpty()) {
                return FALSE.sql();
            }
        }
        return subject.sql() +" IN "+ grouped(range.sql());
    }
}
