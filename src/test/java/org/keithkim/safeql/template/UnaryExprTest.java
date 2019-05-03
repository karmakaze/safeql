package org.keithkim.safeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class UnaryExprTest {
    @Test
    public void resolveSimpleReturnsPrefixedExpr() {
        UnaryExpr<Boolean> notFalse = new UnaryExpr<>("NOT", new Expr<>("FALSE"));
        assertEquals("NOT FALSE", notFalse.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveCompoundReturnsPrefixedGroupedExpr() {
        UnaryExpr<Boolean> notTrueOrFalse = new UnaryExpr<>("NOT", new Expr<>("TRUE OR FALSE"));
        assertEquals("NOT (TRUE OR FALSE)", notTrueOrFalse.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveSimpleFunctionReturnsPrefixedGroupedExpr() {
        UnaryExpr<String> trimPadded = new UnaryExpr<>("TRIM()", new Expr<>("' padded '"));
        assertEquals("TRIM(' padded ')", trimPadded.resolve(emptyMap()).toString());
    }

    @Test
    public void resolveCompoundFunctionReturnsPrefixedGroupedExpr() {
        UnaryExpr<String> trimPrefixSuffix = new UnaryExpr<>("TRIM()", new Expr<>("' prefix ' || ' suffix '"));
        assertEquals("TRIM(' prefix ' || ' suffix ')", trimPrefixSuffix.resolve(emptyMap()).toString());
    }
}
