package org.keithkim.safeql.util;

import org.keithkim.safeql.sql.expression.SqlEntity;
import org.keithkim.safeql.sql.expression.SqlTable;

public class System {
    public static class Table {
        public static SqlTable none = new SqlTable(Table.class);

        public static <E extends SqlEntity> SqlTable<E> none() {
            return (SqlTable<E>) none;
        }

        public static <E extends SqlEntity, T> SqlTable<E>.SqlColumn<T> column(String literal) {
            return (SqlTable<E>.SqlColumn<T>) new SqlTable(Table.class).sqlColumn(literal);
        }
    }
}
