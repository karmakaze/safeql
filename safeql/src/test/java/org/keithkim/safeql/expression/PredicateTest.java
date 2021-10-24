package org.keithkim.safeql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;
import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

public class PredicateTest {
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
}
