package org.keithkim.safeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class NAryExprTest {
    @Test
    public void resolveEmptyReturnsIdentity() {
        NAryExpr<Integer> sumOfNone = new NAryExpr<>("+", new Expr<>("0"));
        assertEquals("0", sumOfNone.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveEmptyFunctionReturnsIdentity() {
        NAryExpr<Boolean> orOfNone = new NAryExpr<>("OR()", new Expr<>("FALSE"));
        assertEquals("FALSE", orOfNone.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveSingleReturnsSelf() {
        NAryExpr<Integer> productOfSingle = new NAryExpr<>("*", new Expr<>("1"), new Expr<>("5"));
        assertEquals("5", productOfSingle.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveSingleFunctionReturnsSelf() {
        NAryExpr<Boolean> andOfSingle = new NAryExpr<>("AND()", new Expr<>("TRUE"), new Expr<>("v"));
        assertEquals("v", andOfSingle.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        NAryExpr<String> aConcatB = new NAryExpr<>("||", new Expr<>("''"), new Expr<>("a"), new Expr<>("b"), new Expr<>("c"));
        assertEquals("a || b || c", aConcatB.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveCompoundFunctionReturnsFunctionArgsGroupedExpr() {
        NAryExpr<String> concatPrefixSuffix = new NAryExpr<>("CONCAT()", new Expr<>("''"), new Expr<>("'a'"), new Expr<>("CONCAT('b', 'c')"));
        assertEquals("CONCAT('a', (CONCAT('b', 'c')))", concatPrefixSuffix.resolve(emptyMap()).toString());
    }
}
