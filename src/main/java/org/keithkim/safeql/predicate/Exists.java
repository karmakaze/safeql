package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.type.Rows;

@EqualsAndHashCode(callSuper = true)
public class Exists extends Predicate {
    private final Expr<? extends Rows> subQuery;

    public Exists(Expr<? extends Rows> subQuery) {
        super("EXISTS");
        this.subQuery = subQuery;
    }

    public String sql() {
        return "EXISTS " + grouped(subQuery.sql());
    }
}
