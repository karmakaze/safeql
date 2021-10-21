package org.keithkim.safeql.schema;

import org.junit.jupiter.api.Test;
import org.keithkim.safeqldemo.projects.*;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ColumnTest {
    @Test
    public void noAliases_selectTerm_returnsTableDotColumnName() {
        Project0.Table projectTable = new Project0.Table("project", null);
        Table<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol(null);

        assertEquals("project.account_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void withTableAlias_selectTerm_returnsAliasDotColumnName() {
        Project0.Table projectTable = new Project0.Table("project", "p");
        Table<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol(null);

        assertEquals("p.account_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void noTableAliasColumnAlias_selectTerm_returnsColumnNameWithAlias() {
        Project0.Table projectTable = new Project0.Table("project", null);
        Table<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol("a_id");

        assertEquals("project.account_id a_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void withTableAlias_selectTerm_returnsQualifiedAlias() {
        Project0.Table projectTable = new Project0.Table("project", "p");
        Table<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol("a_id");

        assertEquals("p.account_id a_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void noAliases_resolve_returnsTableDotColumnName() {
        Project0.Table projectTable = new Project0.Table("project", null);
        Table<Project>.SqlColumn<Long> sqlColumn = projectTable.accountIdCol(null);

        assertEquals("project.account_id", sqlColumn.sql());
    }

    @Test
    public void withTableAlias_resolve_returnsAliasDotColumnName() {
        Project0.Table projectTable = new Project0.Table("project", "p");
        Table<Project>.SqlColumn<Long> sqlColumn = projectTable.accountIdCol(null);

        assertEquals("p.account_id", sqlColumn.sql());
    }

    @Test
    public void noTableAliasColumnAlias_resolve_returnsColumnNameWithAlias() {
        Project0.Table projectTable = new Project0.Table("project", null);
        Table<Project>.SqlColumn<Long> sqlColumn = projectTable.accountIdCol("a_id");

        assertEquals("project.a_id", sqlColumn.sql());
    }

    @Test
    public void withTableAlias_resolve_returnsQualifiedAlias() {
        Project0.Table projectTable = new Project0.Table("project", "p");
        Table<Project>.SqlColumn<Long> sqlColumn = projectTable.accountIdCol("a_id");

        assertEquals("p.a_id", sqlColumn.sql());
    }
}
