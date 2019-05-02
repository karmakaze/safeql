package org.keithkim.typeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class NAryExprTest {
    @Test
    public void renderEmptyReturnsIdentity() {
        NAryExpr<Integer> sumOfNone = new NAryExpr<>("+", new Expr<>("0"));
        assertEquals("0", sumOfNone.render(emptyMap()).toString());
    }

    @Test
    public void renderEmptyFunctionReturnsIdentity() {
        NAryExpr<Boolean> orOfNone = new NAryExpr<>("OR()", new Expr<>("FALSE"));
        assertEquals("FALSE", orOfNone.render(emptyMap()).toString());
    }

    @Test
    public void renderSingleReturnsSelf() {
        NAryExpr<Integer> productOfSingle = new NAryExpr<>("*", new Expr<>("1"), new Expr<>("5"));
        assertEquals("5", productOfSingle.render(emptyMap()).toString());
    }

    @Test
    public void renderSingleFunctionReturnsSelf() {
        NAryExpr<Boolean> andOfSingle = new NAryExpr<>("AND()", new Expr<>("TRUE"), new Expr<>("v"));
        assertEquals("v", andOfSingle.render(emptyMap()).toString());
    }

    @Test
    public void renderSimpleNonAlphaReturnsInfixExpr() {
        NAryExpr<String> aConcatB = new NAryExpr<>("||", new Expr<>("''"), new Expr<>("a"), new Expr<>("b"), new Expr<>("c"));
        assertEquals("a || b || c", aConcatB.render(emptyMap()).toString());
    }

    @Test
    public void renderCompoundFunctionReturnsFunctionArgsGroupedExpr() {
        NAryExpr<String> concatPrefixSuffix = new NAryExpr<>("CONCAT()", new Expr<>("''"), new Expr<>("'a'"), new Expr<>("CONCAT('b', 'c')"));
        assertEquals("CONCAT('a', (CONCAT('b', 'c')))", concatPrefixSuffix.render(emptyMap()).toString());
    }
}
