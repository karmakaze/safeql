package org.keithkim.safeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.template.Expr.expr;

public class SqlBetweenTest {
    @Test
    public void renderSimpleReturnsTernary() {
        SqlBetween<Integer> sqlBetween = new SqlBetween<>(expr("x"), expr("1"), expr("10"));
        assertEquals("x BETWEEN 1 AND 10", sqlBetween.render(emptyMap()).toString());
    }

    @Test
    public void renderCompoundReturnsGroupedTernary() {
        SqlBetween<Integer> sqlBetween = new SqlBetween<>(expr("x + y"), expr("a + b"), expr("c - d"));
        assertEquals("(x + y) BETWEEN (a + b) AND (c - d)", sqlBetween.render(emptyMap()).toString());
    }
}
