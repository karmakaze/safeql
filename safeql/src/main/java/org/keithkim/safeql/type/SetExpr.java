package org.keithkim.safeql.type;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.SqlSet;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;

@EqualsAndHashCode(callSuper = true)
public class SetExpr<T> extends Expr<Set<T>> implements SqlSet<T> {
    private final Collection<? extends Expr<T>> elements;

    public static <T> SetExpr<T> of(Expr<T>... elements) {
        return new SetExpr<>(asList(elements), emptyMap());
    }

    public static <T> SetExpr<T> of(Collection<? extends Expr<T>> elements) {
        return new SetExpr<>(elements, emptyMap());
    }

    public static <T> SetExpr<T> set(String expr) {
        return new SetExpr<T>(expr, emptyMap());
    }

    public SetExpr(String sql, Map<String, Object> bindings) {
        super(sql);
        this.elements = null;
        bindings.forEach((k, v) -> bind(k, v));
    }

    public SetExpr(Collection<? extends Expr<T>> elements, Map<String, Object> bindings) {
        super(null);
        this.elements = elements;
        bindings.forEach((k, v) -> bind(k, v));
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

    public boolean isTerm() {
        return true;
    }

    public String sql() {
        if (elements == null || elements.isEmpty()) {
            return super.isTerm() ? super.sql() : "(" + super.sql() + ")";
        } else if (elements.size() == 1) {
            Expr<T> element = elements.iterator().next();
            return sqlTerm(element);
        } else {
            return "("+ Joiner.on(", ").join(elements.stream().map(Expr::sqlTerm).iterator()) +")";
        }
    }
}
