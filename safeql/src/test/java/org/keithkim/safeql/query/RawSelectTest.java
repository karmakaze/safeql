package org.keithkim.safeql.query;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.schema.Table;
import org.keithkim.safeql.test.Project0;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class RawSelectTest {
    @Test
    public void resolveSimpleReturnsStringAsExpr() {
        Table<Project0> sqlSelect = new Table<>(Project0.class, "SELECT id, name FROM project");
        assertEquals("SELECT id, name FROM project", sqlSelect.sql());
    }
}
