package org.keithkim.safeql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.photos.User;
import org.keithkim.safeql.sql.*;
import org.keithkim.safeql.template.Expr;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class TableSubQueryTest {
    @Test
    void simpleTest() {
        class Photo extends SqlEntity<Long> {
            Photo() {
                super("photo");
            }
        }
        Photo photoEntity = new Photo();

        SqlTable<Photo> photoTable = new SqlTable<>(Photo.class);
        SqlColumn<Photo, Long> idCol = new SqlColumn<>(photoEntity, "id");
        SqlColumn<Photo, Long> userIdCol = new SqlColumn<>(photoEntity, "user_id");

        SqlSelect<Photo> selectQuery = new SqlSelect<>(photoTable, asList(idCol, userIdCol));
        Expr<SqlRows<Photo>> resolved = selectQuery.resolve(emptyMap());
        assertEquals("SELECT id, user_id FROM photo", resolved.toString());
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
