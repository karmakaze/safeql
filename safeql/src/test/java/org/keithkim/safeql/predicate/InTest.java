package org.keithkim.safeql.predicate;

import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.LiteralExpr;
import org.keithkim.safeql.expression.Sql;
import org.keithkim.safeql.expression.SqlSet;
import org.keithkim.safeql.type.SetExpr;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;

public class InTest {
    @Test
    public void resolveSingleReturnsIn() {
        In<Integer> sqlIn = new In<Integer>(expr("x"), SetExpr.of(ImmutableSet.of(expr("10"), expr("20"))));
        assertEquals("x IN (10, 20)", sqlIn.sql());
    }

    @Test
    public void resolveComplexReturnsGroupedIn() {
        LiteralExpr<List<Object>> a10 = new LiteralExpr<>("'a', 10");
        LiteralExpr<List<Object>> b20 = new LiteralExpr<>("'b', 20");
        Collection<LiteralExpr<List<Object>>> ab = ImmutableSet.of(a10, b20);
        In<List<Object>> sqlIn = In.with(Expr.expr("(x, y)"), SetExpr.of(ab));
        assertEquals("(x, y) IN (('a', 10), ('b', 20))", sqlIn.sql());
    }

    @Test
    public void resolveEmptySetReturnsFalse() {
        In<Integer> sqlIn = new In<Integer>(expr("(x, y)"), SetExpr.of(emptyList()));
        assertEquals("FALSE", sqlIn.sql());
    }

    @Test
    public void resolveStringSetReturnsGroupedIn() {
        In<Integer> sqlIn = new In<Integer>(expr("x"), SetExpr.set("1, 2, 3"));
        assertEquals("x IN (1, 2, 3)", sqlIn.sql());
    }

    @Test
    public void resolveEmptyBoundSetReturnsFalse() {
        In<Integer> sqlIn = new In<Integer>(expr("x"), SetExpr.set(":values"));
        sqlIn.bind("values", emptyList());
        assertEquals("FALSE", sqlIn.sql(), "sqlIn.toString: "+ sqlIn.toString());
    }

    @Test
    public void resolveNonEmptyBoundSetReturnsGroupedIn() {
        In<Integer> sqlIn = new In<Integer>(expr("x"), SetExpr.set(":values"));
        sqlIn.bind("values", singletonList(5));
        String expectedSql = String.format("x IN :values_%s", sqlIn.objectId);
        assertEquals(expectedSql, sqlIn.sql());

        String expectedString = String.format("<SQL: x IN :values_%s; BIND: values_%s:[5]>"
                , sqlIn.objectId, sqlIn.objectId);
        assertEquals(expectedString, sqlIn.toString());
    }
}
