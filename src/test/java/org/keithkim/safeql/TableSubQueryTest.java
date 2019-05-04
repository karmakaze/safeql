package org.keithkim.safeql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.sql.*;
import org.keithkim.safeql.template.Expr;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class TableSubQueryTest {
    @Test
    void simpleTest() {
        Project.Table projectTable = new Project.Table("project", null);

        SqlSelect<Project> selectQuery = new SqlSelect<>(projectTable, asList(projectTable.idCol, projectTable.nameCol));
        Expr<SqlRows<Project>> resolved = selectQuery.resolve(emptyMap());
        assertEquals("SELECT project.id, project.name FROM project", resolved.toString());
    }

    @Test
    void nestedTest() {
//        SqlRawSelect<User> subQuery = new SqlRawSelect<>("SELECT * FROM user");

//        SqlRawSelect<SqlRows> subQuery1 = new SqlRawSelect<>("SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6");
//        SelectTableQuery selectTableQuery1 = new SelectTableQuery(new SqlAliasExpr(subQuery1, "t1"));
//        SqlAliasExpr subQuery2 = new SqlAliasExpr(selectTableQuery1, "t2");
//        SelectTableQuery selectTableQuery2 = selectTableQuery1 = new SelectTableQuery(subQuery2);
//
//        assertEquals("SELECT * FROM (SELECT * FROM (SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6) t1) t2",
//                selectTableQuery2.sql());
    }
}
