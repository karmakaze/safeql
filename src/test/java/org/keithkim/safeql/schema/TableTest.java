package org.keithkim.safeql.schema;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TableTest {
    @Test
    public void resoleReturnsTableName() {
        Project.Table projectTable = new Project.Table("photo", null);

        assertEquals("photo", projectTable.sql());
    }

    @Test
    public void resoleReturnsTableNameWithAlias() {
        Project.Table projectTable = new Project.Table("photo", "p");

        assertEquals("photo p", projectTable.sql());
    }

    @Test
    public void resoleReturnsGroupedTableExprWithAlias() {
        Project.Table projectTable = new Project.Table("SELECT * FROM photo", "p");

        assertEquals("(SELECT * FROM photo) p", projectTable.sql());
    }
}
