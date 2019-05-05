package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlRawSelectTest {
    @Test
    public void resolveSimpleReturnsStringAsExpr() {
        SqlRawSelect<Project> sqlSelect = new SqlRawSelect<>(Project.class,
                "SELECT id, name FROM project");
        assertEquals("SELECT id, name FROM project", sqlSelect.sql());
    }
}
