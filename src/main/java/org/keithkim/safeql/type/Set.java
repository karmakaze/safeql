package org.keithkim.safeql.type;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

import java.util.Collection;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

@EqualsAndHashCode(callSuper = true)
public class Set<T> extends Expr<java.util.Set> {
    private final Collection<Expr<T>> elements;

    public static <T> Set<T> of(Expr<T>... elements) {
        return new Set<>(asList(elements));
    }

    public static <T> Set<T> of(Collection<Expr<T>> elements) {
        return new Set<>(elements);
    }

    public static <T> Set<T> set(String expr) {
        return new Set<>(expr);
    }

    public Set(String sql) {
        super(sql);
        this.elements = null;
    }

    public Set(Collection<Expr<T>> elements) {
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
