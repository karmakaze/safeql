package org.keithkim.safeql.schema;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.test.Project0;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TableTest {
    @Test
    public void resoleReturnsTableName() {
        Project0.Table projectTable = new Project0.Table("photo", null);

        assertEquals("photo", projectTable.sql());
    }

    @Test
    public void resoleReturnsTableNameWithAlias() {
        Project0.Table projectTable = new Project0.Table("photo", "p");

        assertEquals("photo p", projectTable.sql());
    }

    @Test
    public void resoleReturnsGroupedTableExprWithAlias() {
        Project0.Table projectTable = new Project0.Table("SELECT * FROM photo", "p");

        assertEquals("(SELECT * FROM photo) p", projectTable.sql());
    }
}
