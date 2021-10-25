package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;

import java.util.List;

import static java.util.stream.Collectors.toList;

public abstract class BinaryExpr<T> extends NAryExpr<T> {
    public BinaryExpr(Expr<T> left, String operator, Expr<T> right) {
        super(expandedSql(operator, null, left, right), operator, null, left, right);
    }

    protected BinaryExpr(String sql, Expr<T> left, String operator, Expr<T> right) {
        super(sql, operator, null, left, right);
    }

//    public Map<String, ?> allBinds() {
//        Map<String, Object> allBinds = (Map<String, Object>) super.allBinds();
//        Map<String, Object> allBind0 = (Map<String, Object>) component(0).allBinds();
//        Map<String, Object> allBind1 = (Map<String, Object>) component(1).allBinds();
//        if (!allBinds.isEmpty() && (!allBind0.isEmpty() || !allBind1.isEmpty())
//                || !allBind0.isEmpty() && !allBind1.isEmpty()) {
//            allBinds = new TreeMap<>(allBinds);
//            allBinds.putAll(allBind0);
//            allBinds.putAll(allBind1);
//            return allBinds;
//        }
//        return ObjectHelpers.firstNonNull(allBinds, allBind0, allBind1);
//    }
}
