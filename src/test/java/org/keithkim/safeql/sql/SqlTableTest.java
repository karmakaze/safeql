package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlTableTest {
    public static class Photo extends SqlEntity {
        public Photo(String tableName) {
            super(tableName);
        }
    }

    @Test
    public void resoleReturnsTableName() {
        SqlTable<Photo> photoTable = new SqlTable<>(Photo.class, "photo");

        Expr<Photo> resolved = photoTable.resolve(emptyMap());
        assertEquals("photo", resolved.toString());
    }
}
