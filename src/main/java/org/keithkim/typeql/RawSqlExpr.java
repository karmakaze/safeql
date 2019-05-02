package org.keithkim.typeql;

public class RawSqlExpr extends SqlExpr {
    public final String sql;

    public RawSqlExpr(String sql) {
        this.sql = sql;
    }

    public String sql() {
        return sql;
    }

    public String toString() {
        return sql();
    }
}
