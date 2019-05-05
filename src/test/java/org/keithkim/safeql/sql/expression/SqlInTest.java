package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.sql.expression.Expr.expr;

public class SqlInTest {
    @Test
    public void resolveSingleReturnsIn() {
        SqlIn<Integer> sqlIn = new SqlIn<>(expr("x"), expr("10"));
        assertEquals("x IN (10)", sqlIn.sql());
    }

    @Test
    public void resolveComplexReturnsGroupedIn() {
        SqlIn<Integer> sqlIn = new SqlIn<>(expr("(x, y)"), expr("('a', 10), ('b', 20)"));
        assertEquals("(x, y) IN (('a', 10), ('b', 20))", sqlIn.sql());
    }
}
