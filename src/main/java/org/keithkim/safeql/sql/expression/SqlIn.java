package org.keithkim.safeql.sql.expression;

import java.util.Set;

import static org.keithkim.safeql.sql.expression.Helpers.grouped;

public class SqlIn<T> extends BinaryPred<T> {
    private final Expr<T> subject;
    private final Expr<Set<T>> range;

    public SqlIn(Expr<T> subject, Expr<Set<T>> range) {
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
        if (range instanceof SqlSet) {
            SqlSet sqlSet = (SqlSet) range;
            if (sqlSet.isKnownEmpty()) {
                return Predicates.FALSE.sql();
            }
        }
        return subject.sql() +" IN "+ grouped(range.sql());
    }
}
