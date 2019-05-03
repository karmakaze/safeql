package org.keithkim.safeql;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TableSubQueryTest {
    @Test
    void literalTest() {
        RawSqlExpr subQuery = new RawSqlExpr("SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6");
        SelectTableQuery selectTableQuery = new SelectTableQuery(new SqlAliasExpr(subQuery, "t"));
        assertEquals("SELECT * FROM (SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6) t", selectTableQuery.sql());
    }

    @Test
    void nestedTest() {
        RawSqlExpr subQuery1 = new RawSqlExpr("SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6");
        SelectTableQuery selectTableQuery1 = new SelectTableQuery(new SqlAliasExpr(subQuery1, "t1"));
        SqlAliasExpr subQuery2 = new SqlAliasExpr(selectTableQuery1, "t2");
        SelectTableQuery selectTableQuery2 = selectTableQuery1 = new SelectTableQuery(subQuery2);

        assertEquals("SELECT * FROM (SELECT * FROM (SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6) t1) t2",
                selectTableQuery2.sql());
    }
}
