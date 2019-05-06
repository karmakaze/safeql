package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;

public class BetweenTest {
    @Test
    public void resolveSimpleReturnsTernary() {
        Between<Integer> sqlBetween = new Between<>(expr("x"), expr("1"), expr("10"));
        assertEquals("x BETWEEN 1 AND 10", sqlBetween.sql());
    }

    @Test
    public void resolveCompoundReturnsGroupedTernary() {
        Between<Integer> sqlBetween = new Between<>(expr("x + y"), expr("a + b"), expr("c - d"));
        assertEquals("(x + y) BETWEEN (a + b) AND (c - d)", sqlBetween.sql());
    }
}
