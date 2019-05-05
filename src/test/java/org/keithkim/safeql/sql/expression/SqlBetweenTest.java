package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.sql.expression.Expr.expr;

public class SqlBetweenTest {
    @Test
    public void resolveSimpleReturnsTernary() {
        SqlBetween<Integer> sqlBetween = new SqlBetween<>(expr("x"), expr("1"), expr("10"));
        assertEquals("x BETWEEN 1 AND 10", sqlBetween.sql());
    }

    @Test
    public void resolveCompoundReturnsGroupedTernary() {
        SqlBetween<Integer> sqlBetween = new SqlBetween<>(expr("x + y"), expr("a + b"), expr("c - d"));
        assertEquals("(x + y) BETWEEN (a + b) AND (c - d)", sqlBetween.sql());
    }
}
