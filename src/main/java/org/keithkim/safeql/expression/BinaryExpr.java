package org.keithkim.safeql.expression;

import org.keithkim.safeql.util.ObjectHelpers;

import java.util.Map;
import java.util.TreeMap;

public abstract class BinaryExpr<T> extends Expr<T> {
    private final NAryExpr<T> nAryExpr;

    public BinaryExpr(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.nAryExpr = new NAryExpr<>(operator, null, left, right);
    }

    public Expr<T> component(int i) {
        return nAryExpr.component(i);
    }

    public Map<String, ?> allBinds() {
        Map<String, Object> allBinds = (Map<String, Object>) super.allBinds();
        Map<String, Object> allBind0 = (Map<String, Object>) component(0).allBinds();
        Map<String, Object> allBind1 = (Map<String, Object>) component(1).allBinds();
        if (!allBinds.isEmpty() && (!allBind0.isEmpty() || !allBind1.isEmpty())
                || !allBind0.isEmpty() && !allBind1.isEmpty()) {
            allBinds = new TreeMap<>(allBinds);
            allBinds.putAll(allBind0);
            allBinds.putAll(allBind1);
            return allBinds;
        }
        return ObjectHelpers.firstNonNull(allBinds, allBind0, allBind1);
    }

    public String sql() {
        return nAryExpr.sql();
    }
}
