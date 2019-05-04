package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlTableTest {
    @Test
    public void resoleReturnsTableName() {
        Project.Table projectTable = new Project.Table("photo", null);

        Expr<Project> resolved = projectTable.resolve(emptyMap());
        assertEquals("photo", resolved.toString());
    }

    @Test
    public void resoleReturnsTableNameWithAlias() {
        Project.Table projectTable = new Project.Table("photo", "p");

        Expr<Project> resolved = projectTable.resolve(emptyMap());
        assertEquals("photo p", resolved.toString());
    }

    @Test
    public void resoleReturnsGroupedTableExprWithAlias() {
        Project.Table projectTable = new Project.Table("SELECT * FROM photo", "p");

        Expr<Project> resolved = projectTable.resolve(emptyMap());
        assertEquals("(SELECT * FROM photo) p", resolved.toString());
    }
}
