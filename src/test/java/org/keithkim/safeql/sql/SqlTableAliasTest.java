package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlTableAliasTest {
    public static class Photo extends SqlEntity {
        public Photo(String tableName) {
            super(tableName);
        }
    }

    @Test
    public void resoleReturnsTableNameWithAlias() {
        SqlTableAlias<Photo> photoTable = new SqlTableAlias<>(Photo.class, "photo", "p");

        Expr<Photo> resolved = photoTable.resolve(emptyMap());
        assertEquals("photo p", resolved.toString());
    }

    @Test
    public void resoleReturnsGroupedTableExprWithAlias() {
        SqlTableAlias<Photo> photoTable = new SqlTableAlias<>(Photo.class, "SELECT * FROM photo", "p");

        Expr<Photo> resolved = photoTable.resolve(emptyMap());
        assertEquals("(SELECT * FROM photo) p", resolved.toString());
    }
}
