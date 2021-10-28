package org.keithkim.safeql.expression;

import java.util.Map;

import static java.util.Collections.emptySet;

public abstract class BinaryExpr<S, L, R> extends Expr<S> {
    private final Sql<L> left;
    private final String operator;
    private final Sql<R> right;

    public BinaryExpr(Sql<L> left, String operator, Sql<R> right) {
        this(expandedSql(left, operator, right), left, operator, right);
    }

    protected BinaryExpr(String sql, Sql<L> left, String operator, Sql<R> right) {
        super(sql, operator.endsWith("()"), emptySet(), true);
        this.left = left;
        this.operator = operator;
        this.right = right;
        for (Sql<?> sqlExpr : new Sql[] {left, right}) {
            for (Map.Entry<String, Object> me : sqlExpr.allBindEntries()) {
                localBinds().put(me.getKey(), me.getValue());
            }
        }
    }

    protected static <L, R> String expandedSql(Sql<L> left, String operator, Sql<R> right) {
        if (operator.endsWith("()")) {
            return operator.substring(0, operator.length()-1) + left.sql() +", "+ right.sql() +")";
        }
        return sqlTerm(left) +" "+ operator +" "+ sqlTerm(right);
    }
}
