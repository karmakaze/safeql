package org.keithkim.typeql;

public class SelectQuery implements SqlExpr {
    public final String sql;

    public SelectQuery(String sql) {
        this.sql = sql;
    }

    public String sql() {
        return sql;
    }

    public String toString() {
        return sql;
    }
}
