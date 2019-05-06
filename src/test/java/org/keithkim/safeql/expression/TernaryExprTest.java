package org.keithkim.safeql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.keithkim.safeql.expression.Expr.expr;

class TernaryExprTest {
    @Test
    void sql() {
        TernaryExpr ternaryExpr = new TernaryExpr(expr("expr1"), "?", expr("expr2"), ":", expr("expr3"));
        assertEquals("expr1 ? expr2 : expr3", ternaryExpr.sql());
    }
}
