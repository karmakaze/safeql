package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlColumnTest {
    @Test
    public void noAliases_selectTerm_returnsTableDotColumnName() {
        Project.Table projectTable = new Project.Table("project", null);
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol(null);

        assertEquals("project.account_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void withTableAlias_selectTerm_returnsAliasDotColumnName() {
        Project.Table projectTable = new Project.Table("project", "p");
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol(null);

        assertEquals("p.account_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void noTableAliasColumnAlias_selectTerm_returnsColumnNameWithAlias() {
        Project.Table projectTable = new Project.Table("project", null);
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol("a_id");

        assertEquals("project.account_id a_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void withTableAlias_selectTerm_returnsQualifiedAlias() {
        Project.Table projectTable = new Project.Table("project", "p");
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol("a_id");

        assertEquals("p.account_id a_id", accountIdCol.selectTerm(emptyMap()));
    }

    @Test
    public void noAliases_resolve_returnsTableDotColumnName() {
        Project.Table projectTable = new Project.Table("project", null);
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol(null);

        Expr<Long> resolved = accountIdCol.resolve(emptyMap());
        assertEquals("project.account_id", resolved.toString());
    }

    @Test
    public void withTableAlias_resolve_returnsAliasDotColumnName() {
        Project.Table projectTable = new Project.Table("project", "p");
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol(null);

        Expr<Long> resolved = accountIdCol.resolve(emptyMap());
        assertEquals("p.account_id", resolved.toString());
    }

    @Test
    public void noTableAliasColumnAlias_resolve_returnsColumnNameWithAlias() {
        Project.Table projectTable = new Project.Table("project", null);
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol("a_id");

        Expr<Long> resolved = accountIdCol.resolve(emptyMap());
        assertEquals("project.a_id", resolved.toString());
    }

    @Test
    public void withTableAlias_resolve_returnsQualifiedAlias() {
        Project.Table projectTable = new Project.Table("project", "p");
        SqlTable<Project>.SqlColumn<Long> accountIdCol = projectTable.accountIdCol("a_id");

        Expr<Long> resolved = accountIdCol.resolve(emptyMap());
        assertEquals("p.a_id", resolved.toString());
    }
}
