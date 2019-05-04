package org.keithkim.safeql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.photos.Photo;
import org.keithkim.demo.photos.User;
import org.keithkim.safeql.Join.Equate;
import org.keithkim.safeql.Join.Type;
import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlTable;
import org.keithkim.safeql.sql.SqlTableAlias;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class JoinTest {
    SqlTable<User> userSqlTable = new SqlTableAlias<>(User.class, "u");
    SqlTable<Photo> photoSqlTable = new SqlTableAlias<>(Photo.class, "p");
    User userEntity = new User(userSqlTable);
    Photo photoEntity = new Photo(photoSqlTable);
    SqlColumn<User, Long> lSqlColumn = userEntity.idCol;
    SqlColumn<Photo, Long> rSqlColumn = photoEntity.userIdCol;

    @Test
    void defaultJoin() {
        Join joinQuery = new Join(userSqlTable, lSqlColumn, photoSqlTable, rSqlColumn);
        assertEquals("user u JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void defaultJoinEquate() {
        Join joinQuery = new Join(userSqlTable, photoSqlTable, new Equate(lSqlColumn, rSqlColumn));
        assertEquals("user u JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void fullJoinEquate() {
        Join joinQuery = new Join(Type.FULL_JOIN, userSqlTable, photoSqlTable, new Equate(lSqlColumn, rSqlColumn));
        assertEquals("user u FULL JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void leftJoinEquate2() {
        Join joinQuery = new Join(Type.LEFT_JOIN, userSqlTable, photoSqlTable, new Equate(lSqlColumn, rSqlColumn), new Equate(lSqlColumn, rSqlColumn));
        assertEquals("user u LEFT JOIN photo p ON u.id = p.user_id AND u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void join() {
        Join joinQuery = new Join(Type.JOIN, userSqlTable, lSqlColumn, photoSqlTable, rSqlColumn);
        assertEquals("user u JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void leftJoin() {
        Join joinQuery = new Join(Type.LEFT_JOIN, userSqlTable, lSqlColumn, photoSqlTable, rSqlColumn);
        assertEquals("user u LEFT JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void rightJoin() {
        Join joinQuery = new Join(Type.RIGHT_JOIN, userSqlTable, lSqlColumn, photoSqlTable, rSqlColumn);
        assertEquals("user u RIGHT JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void fullJoin() {
        Join joinQuery = new Join(Type.FULL_JOIN, userSqlTable, lSqlColumn, photoSqlTable, rSqlColumn);
        assertEquals("user u FULL JOIN photo p ON u.id = p.user_id", joinQuery.resolve(emptyMap()));
    }

    @Test
    void crossJoin() {
        Join joinQuery = new Join(Type.CROSS_JOIN, userSqlTable, lSqlColumn, photoSqlTable, rSqlColumn);
        assertEquals("user u CROSS JOIN photo p", joinQuery.resolve(emptyMap()));
    }
}
