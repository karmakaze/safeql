package org.keithkim.typeql;

public class TableExpr implements SqlExpr {
    public final String expr;
    public final String alias;

    public TableExpr(String expr) {
        this(expr, expr);
    }

    public TableExpr(String expr, String alias) {
        this.expr = expr;
        this.alias = alias;
    }

    public String sql() {
        if (alias == null || alias.equals(expr)) {
            return expr;
        }
        if (expr.contains(" ")) {
            return "("+ expr +") "+ alias;
        }
        return expr +" "+ alias;
    }

    public String toString() {
        return sql();
    }
}
