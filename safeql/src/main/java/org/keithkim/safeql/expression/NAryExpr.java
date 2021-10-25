package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;

import java.util.*;
import java.util.function.Function;

import static java.util.stream.Collectors.toList;

@EqualsAndHashCode(callSuper = true)
public class NAryExpr<T> extends Expr<T> {
    private final String operator;
    private final Expr<T> identity;
    private final Expr<T>[] exprs;

    public NAryExpr(String operator, Expr<T> identity, Expr<T>... exprs) {
        super(expandedSql(operator, identity, exprs), bindingEntries(exprs), false);
        this.operator = operator;
        this.identity = identity;
        this.exprs = exprs;
    }

    protected NAryExpr(String sql, String operator, Expr<T> identity, Expr<T>... exprs) {
        super(sql, bindingEntries(exprs), false);
        this.operator = operator;
        this.identity = identity;
        this.exprs = exprs;
    }

    protected static <T> String expandedSql(String operator, Expr<T> identity, Expr<T>... exprs) {
        if (exprs.length == 0) {
            return identity.sql;
        } else if (operator.endsWith("()")) {
            return operator.substring(0, operator.length()-1) + Joiner.on(", ").join(Arrays.stream(exprs)
                    .map(expr ->Expr.expandTermSql(expr)).collect(toList())) +")";
        } else {
            return Joiner.on(" " + operator + " ").join(Arrays.stream(exprs)
                    .map(expr -> expandTermSql(expr)).collect(toList()));
        }
    }

    protected static <T> Set<Map.Entry<String, Object>> bindingEntries(Expr<T>... exprs) {
        Set<Map.Entry<String, Object>> combined = new HashSet<>();
        for (Expr<T> expr : exprs) {
            if (expr != null) {
                combined.addAll(expr.allBindEntries());
            }
        }
        return combined;
    }

    public Expr<T> component(int i) {
        if (0 <= i && i < exprs.length) {
            return exprs[i];
        }
        return null;
    }

    public String sql() {
        if (exprs.length == 0) {
            return identity.sql();
        } else if (exprs.length == 1) {
            return exprs[0].sql();
        }
        return sql;
//        String operator = super.sql();
//
//        List<String> terms = Arrays.stream(exprs).map(expr -> group(expr.sql())).collect(toList());
//
//        if (operator.endsWith("()")) {
//            String arguments = Joiner.on(", ").join(terms);
//            return operator.substring(0, operator.length() - 1) + arguments +")";
//        } else {
//            return Joiner.on(" "+operator+" ").join(terms);
//        }
    }

    @Override
    public boolean isTerm() {
        if (operator.endsWith("()")) {
            return true;
        } else if (exprs.length == 0) {
            return identity.isTerm();
        } else if (exprs.length == 1) {
            return exprs[0].isTerm();
        }
        return false;
    }
}
