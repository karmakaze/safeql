package org.keithkim.safeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class BinaryExprTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        BinaryExpr<Boolean> trueOrFalse = new BinaryExpr<>(new Expr<>("TRUE"), "OR", new Expr<>("FALSE"));
        assertEquals("TRUE OR FALSE", trueOrFalse.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        BinaryExpr<Boolean> aOrB = new BinaryExpr<>(new Expr<>("a"), "OR", new Expr<>("b"));
        BinaryExpr<Boolean> cOrD = new BinaryExpr<>(new Expr<>("c"), "OR", new Expr<>("d"));
        BinaryExpr<Boolean> aOrBAndCOrD = new BinaryExpr<>(aOrB, "AND", cOrD);
        assertEquals("(a OR b) AND (c OR d)", aOrBAndCOrD.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        BinaryExpr<String> aConcatB = new BinaryExpr<>(new Expr<>("a"), "||", new Expr<>("b"));
        assertEquals("a || b", aConcatB.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        BinaryExpr<String> concatPrefixSuffix = new BinaryExpr<>(new Expr<>("'prefix'"), "CONCAT()", new Expr<>("'suffix'"));
        assertEquals("CONCAT('prefix', 'suffix')", concatPrefixSuffix.resolve(emptyMap()).toString());
    }
}
