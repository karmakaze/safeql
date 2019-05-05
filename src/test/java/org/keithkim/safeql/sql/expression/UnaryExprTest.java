package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.sql.expression.UnaryExpr;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.sql.expression.Expr.expr;

public class UnaryExprTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        UnaryExpr<Boolean> notFalse = new UnaryExpr<>("NOT", Predicates.FALSE);
        assertEquals("NOT FALSE", notFalse.sql());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        UnaryExpr<Boolean> notTrueOrFalse = new UnaryExpr<>("NOT", expr("TRUE OR FALSE"));
        assertEquals("NOT (TRUE OR FALSE)", notTrueOrFalse.sql());
    }

    @Test
    public void resolveSimpleFunctionReturnsPrefixedGroupedExpr() {
        UnaryExpr<String> trimPadded = new UnaryExpr<>("TRIM()", expr("' padded '"));
        assertEquals("TRIM(' padded ')", trimPadded.sql());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        UnaryExpr<String> trimPrefixSuffix = new UnaryExpr<>("TRIM()", expr("' prefix ' || ' suffix '"));
        assertEquals("TRIM(' prefix ' || ' suffix ')", trimPrefixSuffix.sql());
    }
}
