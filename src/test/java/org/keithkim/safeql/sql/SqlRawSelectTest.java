package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.template.Expr.expr;

public class SqlRawSelectTest {
    @Test
    public void resolveSimpleReturnsStringAsExpr() {
        SqlRawSelect<Rows> sqlSelect = new SqlRawSelect<>("SELECT 1, 2, 3");
        Expr<Rows> resolved = sqlSelect.resolve(emptyMap());
        assertEquals("SELECT 1, 2, 3", resolved.toString());
    }
}
