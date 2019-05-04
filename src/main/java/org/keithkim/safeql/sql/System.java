package org.keithkim.safeql.sql;

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
