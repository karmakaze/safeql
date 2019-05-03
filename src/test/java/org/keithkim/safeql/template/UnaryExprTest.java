package org.keithkim.safeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class UnaryExprTest {
    @Test
    public void renderSimpleReturnsPrefixedExpr() {
        UnaryExpr<Boolean> notFalse = new UnaryExpr<>("NOT", new Expr<>("FALSE"));
        assertEquals("NOT FALSE", notFalse.render(emptyMap()).toString());
    }

    @Test
    public void renderCompoundReturnsPrefixedGroupedExpr() {
        UnaryExpr<Boolean> notTrueOrFalse = new UnaryExpr<>("NOT", new Expr<>("TRUE OR FALSE"));
        assertEquals("NOT (TRUE OR FALSE)", notTrueOrFalse.render(emptyMap()).toString());
    }

    @Test
    public void renderSimpleFunctionReturnsPrefixedGroupedExpr() {
        UnaryExpr<String> trimPadded = new UnaryExpr<>("TRIM()", new Expr<>("' padded '"));
        assertEquals("TRIM(' padded ')", trimPadded.render(emptyMap()).toString());
    }

    @Test
    public void renderCompoundFunctionReturnsPrefixedGroupedExpr() {
        UnaryExpr<String> trimPrefixSuffix = new UnaryExpr<>("TRIM()", new Expr<>("' prefix ' || ' suffix '"));
        assertEquals("TRIM(' prefix ' || ' suffix ')", trimPrefixSuffix.render(emptyMap()).toString());
    }
}
