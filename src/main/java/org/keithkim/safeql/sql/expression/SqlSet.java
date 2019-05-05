package org.keithkim.safeql.sql.expression;

import com.google.common.base.Joiner;

import java.util.Collection;
import java.util.Set;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.keithkim.safeql.sql.expression.Helpers.grouped;

public class SqlSet<T> extends Expr<Set<T>> {
    private final Collection<Expr<T>> elements;

    public static <T> SqlSet<T> of(Expr<T>... elements) {
        return new SqlSet<>(asList(elements));
    }

    public static <T> SqlSet<T> of(Collection<Expr<T>> elements) {
        return new SqlSet<>(elements);
    }

    public static <T> SqlSet<T> set(String expr) {
        return new SqlSet<>(expr);
    }

    public SqlSet(String sql) {
        super(sql);
        this.elements = null;
    }

    public SqlSet(Collection<Expr<T>> elements) {
        super(null);
        this.elements = elements;
    }

    public boolean isKnownEmpty() {
        if (elements == null) {
            Object value = super.eval();
            if (value instanceof Collection) {
                if (((Collection) value).isEmpty()) {
                    return true;
                }
            }
        } else if (elements.isEmpty()) {
            return true;
        }
        return false;
    }

    public boolean isKnownNonEmpty() {
        if (elements == null) {
            Object value = super.eval();
            if (value instanceof Collection) {
                if (!((Collection) value).isEmpty()) {
                    return true;
                }
            }
        } else if (!elements.isEmpty()) {
            return true;
        }
        return false;
    }

    public String sql() {
        if (elements == null) {
            return grouped(super.sql());
        } else {
            return grouped(Joiner.on(", ").join(elements.stream().map(e -> e.sql()).collect(toList())));
        }
    }
}
