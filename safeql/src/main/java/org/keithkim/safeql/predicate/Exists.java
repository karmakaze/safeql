package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.expression.UnaryPredicate;
import org.keithkim.safeql.type.Rows;

import java.util.Map;
import java.util.Set;

@EqualsAndHashCode(callSuper = true)
public class Exists<T extends Rows> extends UnaryPredicate<T> implements Predicate {
    public Exists(Expr<T> subQuery, Set<Map.Entry<String, Object>> bindEntries) {
        super("EXISTS", subQuery, bindEntries);
    }

//    public String sql() {
//        return "EXISTS " + grouped(subQuery.sql());
//    }
}
