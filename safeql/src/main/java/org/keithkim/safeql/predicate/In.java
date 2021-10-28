package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.Sql;
import org.keithkim.safeql.expression.SqlSet;
import org.keithkim.safeql.type.SetExpr;

import java.util.Set;

import static org.keithkim.safeql.predicate.Predicates.FALSE;

@EqualsAndHashCode(callSuper = true)
public class In<T> extends BinaryPredicate<T, Set<T>> {
    private final Sql<T> subject;
    private final SqlSet<T> range;

    public static <T> In<T> with(Sql<T> subject, SqlSet<T> range) {
        return new In<T>(subject, range);
    }

    public In(Sql<T> subject, SqlSet<T> range) {
        super(expandedSql(subject, range), subject, "IN", range);
        this.subject = subject;
        this.range = range;
    }

    @Override
    public void bind(String name, Object value) {
        super.bind(name, value);
//        subject.bind(name, value);
        if (range instanceof SetExpr) {
            ((SetExpr) range).bind(name, value);
        }
    }

    @Override
    public boolean isTerm() {
        return false;
    }

    public String sql() {
        if (range instanceof SetExpr) {
            SetExpr<T> sqlSet = (SetExpr) range;
            if (sqlSet.isKnownEmpty()) {
                return FALSE.sql();
            }
        }
        return super.sql();
    }

    protected static <T> String expandedSql(Sql<T> subject, SqlSet<T> range) {
        if (range instanceof SetExpr) {
            SetExpr<T> sqlSet = (SetExpr) range;
            if (sqlSet.isKnownEmpty()) {
                return FALSE.sql();
            }
        }
        return subject.sql() +" IN "+ sqlTerm(range);
    }

//    public Set<Map.Entry<String, Object>> allBindEntries() {
//        return Sets.union(subject.allBindEntries(), range.allBindEntries());
//    }
}
