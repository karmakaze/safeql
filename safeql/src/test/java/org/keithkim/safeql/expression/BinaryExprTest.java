package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.TreeMap;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.matchesPattern;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;
import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;
import static org.keithkim.safeql.test.TestHelpers.assertMatches;

public class BinaryExprTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        BinaryExpr<Boolean, Boolean, Boolean> trueOrFalse = new BinaryExpr<Boolean, Boolean, Boolean>(TRUE, "OR", FALSE) {};
        assertEquals("TRUE OR FALSE", trueOrFalse.sql());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        BinaryExpr<Boolean, Boolean, Boolean> aOrB = new BinaryExpr<Boolean, Boolean, Boolean>(expr("a"), "OR", expr("b")) {};
        BinaryExpr<Boolean, Boolean, Boolean> cOrD = new BinaryExpr<Boolean, Boolean, Boolean>(expr("c"), "OR", expr("d")) {};
        BinaryExpr<Boolean, Boolean, Boolean> aOrBAndCOrD = new BinaryExpr<Boolean, Boolean, Boolean>(aOrB, "AND", cOrD) {};
        assertEquals("(a OR b) AND (c OR d)", aOrBAndCOrD.sql());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        BinaryExpr<String, String, String> aConcatB = new BinaryExpr<String, String, String>(expr("a"), "||", expr("b")) {};
        assertEquals("a || b", aConcatB.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        BinaryExpr<String, String, String> concatPrefixSuffix = new BinaryExpr<String, String, String>(expr("'prefix'"), "CONCAT()", expr("'suffix'")) {};
        assertEquals("CONCAT('prefix', 'suffix')", concatPrefixSuffix.sql());
    }

    @Test
    void localBoundExpr_bindComposite_shouldNotChangeNestedLocalBinds() {
        Expr<Boolean> expr1 = Expr.expr("x BETWEEN ? AND ?", 1, 2);
        String expectExpr1 = String.format("<SQL: x BETWEEN :_1_%s AND :_2_%s; BIND: _1_%s:1, _2_%s:2>",
                expr1.objectId, expr1.objectId, expr1.objectId, expr1.objectId);
        assertEquals(expectExpr1, expr1.toString());

        Expr<Boolean> expr2 = Expr.expr("y BETWEEN :y1 AND :y2", ImmutableMap.of("y1", 3, "y2", 4));
        String expectExpr2 = String.format("<SQL: y BETWEEN :y1_%s AND :y2_%s; BIND: y1_%s:3, y2_%s:4>",
                expr2.objectId, expr2.objectId, expr2.objectId, expr2.objectId);
        assertEquals(expectExpr2, expr2.toString());

        Expr<Boolean> expr = new BinaryExpr(expr1, "AND", expr2) {};
        String expectExpr = String.format("<SQL: (x BETWEEN :_1_%s AND :_2_%s) AND (y BETWEEN :y1_%s AND :y2_%s); "+
                        "BIND: _1_%s:1, _2_%s:2, y1_%s:3, y2_%s:4>",
                expr1.objectId, expr1.objectId, expr2.objectId, expr2.objectId,
                expr1.objectId, expr1.objectId, expr2.objectId, expr2.objectId);
        assertEquals(expectExpr, expr.toString());

        Map<String, Object> binds = new TreeMap<>();
        for (int i = 1; i <= 2; i++) {
            binds.put("_"+ i +"_" + expr1.objectId, i);
        }
        binds.put("y1_"+expr2.objectId, 3);
        binds.put("y2_"+expr2.objectId, 4);
        String sql = String.format("(x BETWEEN :%s AND :%s) AND (y BETWEEN :%s AND :%s)",
                binds.keySet().toArray());
        String bind = Joiner.on(", ").withKeyValueSeparator(":").join(binds);
        String expected = "<SQL: " + sql + "; BIND: " + bind + ">";

        assertEquals(expected, expr.toString());
    }

    @Test
    void partialBinding_thenComposing_lateBinding_hasIndependentOriginalBindings() {
        Expr<Boolean> expr1 = Expr.expr("x BETWEEN ? AND ?", 1);
        String expectExpr1 = String.format("<SQL: x BETWEEN :_1_%s AND ?; BIND: _1_%s:1>",
                expr1.objectId, expr1.objectId);
        assertEquals(expectExpr1, expr1.toString());

        Expr<Boolean> expr2 = Expr.expr("y BETWEEN :y1 AND :y2", ImmutableMap.of("y2", 4));
        String expectExpr2 = String.format("<SQL: y BETWEEN :y1 AND :y2_%s; BIND: y2_%s:4>",
                expr2.objectId, expr2.objectId);
        assertEquals(expectExpr2, expr2.toString());

        Expr<Boolean> expr = new BinaryExpr(expr1, "AND", expr2) {};
        String expectExpr = String.format("<SQL: (x BETWEEN :_1_%s AND ?) AND (y BETWEEN :y1 AND :y2_%s); "+
                        "BIND: _1_%s:1, y2_%s:4>",
                expr1.objectId, expr2.objectId, expr1.objectId, expr2.objectId);
        assertEquals(expectExpr, expr.toString());

        expr.bind("?", 5);
        expr.bind("y1", 6);
        expr.bind("y2", 7);

        assertEquals(expectExpr1, expr1.toString());
        assertEquals(expectExpr2, expr2.toString());
        expectExpr = "<SQL: \\(x BETWEEN :_1_[a-z0-9]+ AND :_1_[a-z0-9]+\\) AND "+
                           "\\(y BETWEEN :y1_[a-z0-9]+ AND :y2_[a-z0-9]+\\); "+
                       "BIND: _1_[a-z0-9]+:1, y2_[a-z0-9]+:4, _1_[a-z0-9]+:5, y1_[a-z0-9]+:6>";
        assertThat(expr.toString(), matchesPattern(expectExpr));
    }
}
