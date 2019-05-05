package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlRawSelectTest {
    @Test
    public void resolveSimpleReturnsStringAsExpr() {
        SqlRawSelect<Project> sqlSelect = new SqlRawSelect<>(Project.class,
                "SELECT id, name FROM project");
        Expr<SqlRows<Project>> resolved = sqlSelect.resolve(emptyMap());
        assertEquals("SELECT id, name FROM project", resolved.toString());
    }
}
