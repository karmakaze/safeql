package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.sql.expression.NAryExpr;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.sql.expression.Expr.expr;

public class NAryExprTest {
    @Test
    public void resolveEmptyReturnsIdentity() {
        NAryExpr<Integer> sumOfNone = new NAryExpr<>("+", expr("0"));
        assertEquals("0", sumOfNone.sql());
    }

    @Test
    public void resolveEmptyFunctionReturnsIdentity() {
        NAryExpr<Boolean> orOfNone = new NAryExpr<>("OR()", expr("FALSE"));
        assertEquals("FALSE", orOfNone.sql());
    }

    @Test
    public void resolveSingleReturnsSelf() {
        NAryExpr<Integer> productOfSingle = new NAryExpr<>("*", expr("1"), expr("5"));
        assertEquals("5", productOfSingle.sql());
    }

    @Test
    public void resolveSingleFunctionReturnsSelf() {
        NAryExpr<Boolean> andOfSingle = new NAryExpr<>("AND()", expr("TRUE"), expr("v"));
        assertEquals("v", andOfSingle.sql());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        NAryExpr<String> aConcatB = new NAryExpr<>("||", expr("''"), expr("a"), expr("b"), expr("c"));
        assertEquals("a || b || c", aConcatB.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsFunctionArgsGroupedExpr() {
        NAryExpr<String> concatPrefixSuffix = new NAryExpr<>("CONCAT()", expr("''"), expr("'a'"), expr("CONCAT('b', 'c')"));
        assertEquals("CONCAT('a', (CONCAT('b', 'c')))", concatPrefixSuffix.sql());
    }
}
