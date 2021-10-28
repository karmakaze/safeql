package org.keithkim.safeql.predicate;

import org.keithkim.safeql.expression.BinaryExpr;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.Sql;

public class BinaryPredicate<L, R> extends BinaryExpr<Boolean, L, R> implements Predicate {
    public BinaryPredicate(Sql<L> left, String operator, Sql<R> right) {
        super(left, operator, right);
    }

    protected BinaryPredicate(String sql, Sql<L> left, String operator, Sql<R> right) {
        super(sql, left, operator, right);
    }

//    @Override
//    public Object eval() {
//        return null;
//    }
}
