package org.keithkim.typeql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.photos.Photo;
import org.keithkim.demo.photos.User;
import org.keithkim.typeql.Join.Equate;
import org.keithkim.typeql.Join.Type;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class JoinTest {
    Table<User> userTable = new Table<>(User.class, "u");
    Table<Photo> photoTable = new Table<>(Photo.class, "p");
    User userEntity = new User(userTable);
    Photo photoEntity = new Photo(photoTable);
    Col<User, Long> lCol = userEntity.idCol;
    Col<Photo, Long> rCol = photoEntity.userIdCol;

    @Test
    void defaultJoin() {
        Join joinQuery = new Join(userTable, lCol, photoTable, rCol);
        assertEquals("user u JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void defaultJoinEquate() {
        Join joinQuery = new Join(userTable, photoTable, new Equate(lCol, rCol));
        assertEquals("user u JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void fullJoinEquate() {
        Join joinQuery = new Join(Type.FULL_JOIN, userTable, photoTable, new Equate(lCol, rCol));
        assertEquals("user u FULL JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void leftJoinEquate2() {
        Join joinQuery = new Join(Type.LEFT_JOIN, userTable, photoTable, new Equate(lCol, rCol), new Equate(lCol, rCol));
        assertEquals("user u LEFT JOIN photo p ON u.id = p.user_id AND u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void join() {
        Join joinQuery = new Join(Type.JOIN, userTable, lCol, photoTable, rCol);
        assertEquals("user u JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void leftJoin() {
        Join joinQuery = new Join(Type.LEFT_JOIN, userTable, lCol, photoTable, rCol);
        assertEquals("user u LEFT JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void rightJoin() {
        Join joinQuery = new Join(Type.RIGHT_JOIN, userTable, lCol, photoTable, rCol);
        assertEquals("user u RIGHT JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void fullJoin() {
        Join joinQuery = new Join(Type.FULL_JOIN, userTable, lCol, photoTable, rCol);
        assertEquals("user u FULL JOIN photo p ON u.id = p.user_id", joinQuery.sql());
    }

    @Test
    void crossJoin() {
        Join joinQuery = new Join(Type.CROSS_JOIN, userTable, lCol, photoTable, rCol);
        assertEquals("user u CROSS JOIN photo p", joinQuery.sql());
    }
}
