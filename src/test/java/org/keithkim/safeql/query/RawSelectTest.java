package org.keithkim.safeql.query;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.query.RawSelect;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class RawSelectTest {
    @Test
    public void resolveSimpleReturnsStringAsExpr() {
        RawSelect<Project> sqlSelect = new RawSelect<>(Project.class,
                "SELECT id, name FROM project");
        assertEquals("SELECT id, name FROM project", sqlSelect.sql());
    }
}
