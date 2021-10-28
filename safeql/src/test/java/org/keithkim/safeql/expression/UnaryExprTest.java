package org.keithkim.safeql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.expression.Expr.expr;
import static org.keithkim.safeql.predicate.Predicates.FALSE;

public class UnaryExprTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        UnaryPredicate<Boolean> notFalse = new UnaryPredicate<Boolean>("NOT", FALSE) {};
        assertEquals("NOT FALSE", notFalse.sql());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        UnaryPredicate<Boolean> notTrueOrFalse = new UnaryPredicate<Boolean>("NOT", expr("TRUE OR FALSE")) {};
        assertEquals("NOT (TRUE OR FALSE)", notTrueOrFalse.sql());
    }

    @Test
    public void resolveSimpleFunctionReturnsPrefixedGroupedExpr() {
        UnaryPredicate<String> trimPadded = new UnaryPredicate<String>("TRIM()", expr("' padded '")) {};
        assertEquals("TRIM(' padded ')", trimPadded.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        UnaryPredicate<String> trimPrefixSuffix = new UnaryPredicate<String>("TRIM()", expr("' prefix ' || ' suffix '")) {};
        assertEquals("TRIM(' prefix ' || ' suffix ')", trimPrefixSuffix.sql());
    }
}
