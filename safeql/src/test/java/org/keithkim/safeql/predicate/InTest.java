package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.type.Set;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;

public class InTest {
    @Test
    public void resolveSingleReturnsIn() {
        In<Integer> sqlIn = new In<>(expr("x"), expr("10"));
        assertEquals("x IN (10)", sqlIn.sql());
    }

    @Test
    public void resolveComplexReturnsGroupedIn() {
        In<Integer> sqlIn = new In<>(expr("(x, y)"), expr("('a', 10), ('b', 20)"));
        assertEquals("(x, y) IN (('a', 10), ('b', 20))", sqlIn.sql());
    }

    @Test
    public void resolveEmptySetReturnsFalse() {
        In<Integer> sqlIn = new In<>(expr("(x, y)"), new Set(emptyList()));
        assertEquals("FALSE", sqlIn.sql());
    }

    @Test
    public void resolveStringSetReturnsGroupedIn() {
        In<Integer> sqlIn = new In<>(expr("x"), new Set("1, 2, 3"));
        assertEquals("x IN (1, 2, 3)", sqlIn.sql());
    }

    @Test
    public void resolveEmptyBoundSetReturnsFalse() {
        In<Integer> sqlIn = new In<>(expr("x"), new Set(":values"));
        sqlIn.bind("values", emptyList());
        assertEquals("FALSE", sqlIn.sql(), "sqlIn.toString: "+ sqlIn.toString());
    }

    @Test
    public void resolveNonEmptyBoundSetReturnsGroupedIn() {
        In<Integer> sqlIn = new In<>(expr("x"), new Set(":values"));
        sqlIn.bind("values", singletonList(5));
        assertEquals("x IN (:values)", sqlIn.sql(), "sqlIn.toString: "+ sqlIn.toString());
    }
}
