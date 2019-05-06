package org.keithkim.safeql.schema;

public class Sys {
    public static class Table {
        public static org.keithkim.safeql.schema.Table none = new org.keithkim.safeql.schema.Table(Table.class);

        public static <E extends Entity> org.keithkim.safeql.schema.Table<E> none() {
            return (org.keithkim.safeql.schema.Table<E>) none;
        }

        public static <E extends Entity, T> org.keithkim.safeql.schema.Table<E>.SqlColumn<T> column(String literal) {
            return (org.keithkim.safeql.schema.Table<E>.SqlColumn<T>) new org.keithkim.safeql.schema.Table(Table.class).sqlColumn(literal);
        }
    }
}
