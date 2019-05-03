package org.keithkim.safeql.template;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ExprTest {
    @Test
    public void toStringReturnsString() {
        Expr<String> expr = new Expr<>("This is a string expression.");
        assertEquals("This is a string expression.", expr.toString());
    }

    @Test
    public void resolveReturnsStringExpr() {
        Expr<String> expr = new Expr<>("This is a string expression.");
        Expr<String> rendered = expr.resolve(emptyMap());
        assertEquals("This is a string expression.", rendered.toString());
    }
}
