package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.sql.expression.Expr.expr;

public class BinaryPredTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        BinaryExpr<Boolean> trueOrFalse = new BinaryExpr<>(Predicates.TRUE, "OR", Predicates.FALSE);
        assertEquals("TRUE OR FALSE", trueOrFalse.sql());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        BinaryExpr<Boolean> aOrB = new BinaryExpr<>(expr("a"), "OR", expr("b"));
        BinaryExpr<Boolean> cOrD = new BinaryExpr<>(expr("c"), "OR", expr("d"));
        BinaryExpr<Boolean> aOrBAndCOrD = new BinaryExpr<>(aOrB, "AND", cOrD);
        assertEquals("(a OR b) AND (c OR d)", aOrBAndCOrD.sql());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        BinaryExpr<String> aConcatB = new BinaryExpr<>(expr("a"), "||", expr("b"));
        assertEquals("a || b", aConcatB.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        BinaryExpr<String> concatPrefixSuffix = new BinaryExpr<>(expr("'prefix'"), "CONCAT()", expr("'suffix'"));
        assertEquals("CONCAT('prefix', 'suffix')", concatPrefixSuffix.sql());
    }
}
