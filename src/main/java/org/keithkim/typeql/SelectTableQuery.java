package org.keithkim.typeql;

public class SelectTableQuery extends SqlAliasExpr {
    public final SqlAliasExpr tableExpr;

    public SelectTableQuery(SqlAliasExpr tableExpr) {
        super(tableExpr, tableExpr.alias);
        this.tableExpr = tableExpr;
    }

    public String sql() {
        return "SELECT * FROM "+ tableExpr.sql();
    }

    public String toString() {
        return sql();
    }
}
