package org.keithkim.safeql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;
import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

public class PredicateTest {
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
}
