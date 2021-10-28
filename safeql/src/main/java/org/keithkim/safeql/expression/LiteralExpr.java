package org.keithkim.safeql.expression;

import java.util.Map;
import java.util.Set;

import static java.util.Collections.emptySet;

public class LiteralExpr<T> extends Expr<T> {
    public LiteralExpr(String sql) {
        super(sql, sql.indexOf(' ') < 0, emptySet(), true);
    }
    public LiteralExpr(String sql, Set<Map.Entry<String, Object>> bindEntries) {
        super(sql, true, bindEntries, true);
    }
}
