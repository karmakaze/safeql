package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlColumnAliasTest {
    public static class Photo extends SqlEntity {
        public Photo(String tableName) {
            super(tableName);
        }
    }

    @Test
    public void resoleReturnsColumnNameWithAlias() {
        Photo photoEntity = new Photo("photo");

        SqlColumnAlias<Photo, Long> photoIdCol = new SqlColumnAlias<>(photoEntity, "id", "photoId");

        Expr<Long> resolved = photoIdCol.resolve(emptyMap());
        assertEquals("id photoId", resolved.toString());
    }

    @Test
    public void resoleReturnsGroupedColumnExprWithAlias() {
        Photo photoEntity = new Photo("photo");
        SqlColumnAlias<Photo, Long> photoIdCol = new SqlColumnAlias<>(photoEntity, "SELECT id FROM photo LIMIT 1", "photoId");

        Expr<Long> resolved = photoIdCol.resolve(emptyMap());
        assertEquals("(SELECT id FROM photo LIMIT 1) photoId", resolved.toString());
    }
}
