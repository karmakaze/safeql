package org.keithkim.safeql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;
import static org.keithkim.safeql.predicate.Predicates.FALSE;
import static org.keithkim.safeql.predicate.Predicates.TRUE;

public class NAryExprTest {
    @Test
    public void resolveEmptyReturnsIdentity() {
        NAryExpr<Integer, Integer> sumOfNone = new NAryExpr<Integer, Integer>("+", expr("0"));
        assertEquals("0", sumOfNone.sql());
    }

    @Test
    public void resolveEmptyFunctionReturnsIdentity() {
        NAryExpr<Boolean, Boolean> orOfNone = new NAryExpr<Boolean, Boolean>("OR()", expr("FALSE"));
        assertEquals("FALSE", orOfNone.sql());
    }

    @Test
    public void resolveSingleReturnsSelf() {
        NAryExpr<Integer, Integer> productOfSingle = new NAryExpr<Integer, Integer>("*", expr("1"), expr("5"));
        assertEquals("5", productOfSingle.sql());
    }

    @Test
    public void resolveSingleFunctionReturnsSelf() {
        NAryExpr<Boolean, Boolean> andOfSingle = new NAryExpr<Boolean, Boolean>("AND(TRUE, v)", "AND()", expr("TRUE"), expr("v"));
        assertEquals("v", andOfSingle.sql());
    }

    @Test
    public void resolveSimpleNonAlphaReturnsInfixExpr() {
        NAryExpr<String, String> aConcatB = new NAryExpr<String, String>("||", expr("''"), expr("a"), expr("b"), expr("c"));
        assertEquals("a || b || c", aConcatB.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsFunctionArgsGroupedExpr() {
        NAryExpr<String, String> concatPrefixSuffix = new NAryExpr<String, String>("CONCAT()", expr("''"), expr("'a'"), expr("CONCAT('b', 'c')"));
        assertEquals("CONCAT('a', (CONCAT('b', 'c')))", concatPrefixSuffix.sql());
    }

    @Test
    public void componentWithValidIndex_returnsExpr() {
        NAryExpr<String, String> concatABC = new NAryExpr<String, String>("||", expr("''"), expr("a"), expr("b"), expr("c"));
        assertEquals(expr("a"), concatABC.component(0));
        assertEquals(expr("b"), concatABC.component(1));
        assertEquals(expr("c"), concatABC.component(2));
    }

    @Test
    public void componentWithNegativeIndex_returnsNull() {
        NAryExpr<String, String> concatABC = new NAryExpr<String, String>("||", expr("''"), expr("a"), expr("b"), expr("c"));
        assertEquals(null, concatABC.component(-1));
    }

    @Test
    public void componentWithOutOfBoundIndex_returnsNull() {
        NAryExpr<String, String> concatABC = new NAryExpr<String, String>("||", expr("''"), expr("a"), expr("b"), expr("c"));
        assertEquals(null, concatABC.component(3));
    }
}
