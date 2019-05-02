package org.keithkim.typeql;

public class SqlAliasExpr extends SqlExpr {
    public final SqlExpression sqlExpr;
    public final String alias;

    public SqlAliasExpr(SqlExpression sqlExpr, String alias) {
        this.sqlExpr = sqlExpr;
        this.alias = alias;
    }

    public String sql() {
        String sql = sqlExpr.sql();
        if (sql.contains(" ")) {
            return "("+ sql +") "+ alias;
        }
        return sql +" "+ alias;
    }

    public String toString() {
        return sql();
    }
}
