package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
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

    @Test
    public void resolveEmptySetReturnsFalse() {
        SqlIn<Integer> sqlIn = new SqlIn<>(expr("(x, y)"), new SqlSet(emptyList()));
        assertEquals("FALSE", sqlIn.sql());
    }

    @Test
    public void resolveStringSetReturnsGroupedIn() {
        SqlIn<Integer> sqlIn = new SqlIn<>(expr("x"), new SqlSet("1, 2, 3"));
        assertEquals("x IN (1, 2, 3)", sqlIn.sql());
    }

    @Test
    public void resolveEmptyBoundSetReturnsFalse() {
        SqlIn<Integer> sqlIn = new SqlIn<>(expr("x"), new SqlSet(":values"));
        sqlIn.bind("values", emptyList());
        assertEquals("FALSE", sqlIn.sql(), "sqlIn.toString: "+ sqlIn.toString());
    }

    @Test
    public void resolveNonEmptyBoundSetReturnsGroupedIn() {
        SqlIn<Integer> sqlIn = new SqlIn<>(expr("x"), new SqlSet(":values"));
        sqlIn.bind("values", singletonList(5));
        assertEquals("x IN (:values)", sqlIn.sql(), "sqlIn.toString: "+ sqlIn.toString());
    }
}
