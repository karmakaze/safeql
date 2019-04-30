package org.keithkim.typeql;

public class Query {
    public class Select<T extends Entity> {
    }

    public class SelectColumns {
    }

    public class SelectColumnsFromTable {
    }

    public class SelectFromTable {
    }

    public static <T extends Entity> Select<T> from(T entity) {
        return null;
    }
}

// query states
// select: columns, from-table
// select.columns: from-table
// select.columns.from-table: execute
