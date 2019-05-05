package org.keithkim.safeql;


import org.keithkim.safeql.sql.expression.Expr;

public class SelectQuery extends Expr<String> {
    public SelectQuery(String sql) {
        super(sql);
    }
}
