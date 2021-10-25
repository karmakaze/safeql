package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;
import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

public class BinaryExprTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        BinaryExpr<Boolean> trueOrFalse = new BinaryExpr<Boolean>(TRUE, "OR", FALSE) {};
        assertEquals("TRUE OR FALSE", trueOrFalse.sql());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        BinaryExpr<Boolean> aOrB = new BinaryExpr<Boolean>(expr("a"), "OR", expr("b")) {};
        BinaryExpr<Boolean> cOrD = new BinaryExpr<Boolean>(expr("c"), "OR", expr("d")) {};
        BinaryExpr<Boolean> aOrBAndCOrD = new BinaryExpr<Boolean>(aOrB, "AND", cOrD) {};
        assertEquals("(a OR b) AND (c OR d)", aOrBAndCOrD.sql());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        BinaryExpr<String> aConcatB = new BinaryExpr<String>(expr("a"), "||", expr("b")) {};
        assertEquals("a || b", aConcatB.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        BinaryExpr<String> concatPrefixSuffix = new BinaryExpr<String>(expr("'prefix'"), "CONCAT()", expr("'suffix'")) {};
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

        expr.bind("x1", 5);
        expr.bind("x2", 6);
        expr.bind("?", 7);
        expr.bind("?", 8);

        assertEquals(expected, expr.toString());
    }
}
