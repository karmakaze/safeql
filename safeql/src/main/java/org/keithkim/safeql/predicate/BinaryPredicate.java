package org.keithkim.safeql.predicate;

import com.google.common.collect.Sets;
import org.keithkim.safeql.expression.BinaryExpr;
import org.keithkim.safeql.expression.Expr;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.SortedMap;

public abstract class BinaryPredicate<T> extends Predicate {
    private final BinaryExpr<T> binaryExpr;

    public BinaryPredicate(Expr<T> left, String operator, Expr<T> right) {
        super(null);
        this.binaryExpr = new BinaryExpr<T>(left, operator, right) {};
    }

    protected BinaryPredicate(String sql, Expr<T> left, String operator, Expr<T> right) {
        super(sql);
        this.binaryExpr = new BinaryExpr<T>(sql, left, operator, right) {};
    }

    protected Expr<T> left() {
        return binaryExpr.component(0);
    }

    protected Expr<T> right() {
        return binaryExpr.component(1);
    }

    public String sql() {
        return binaryExpr.sql();
    }

    public Set<Map.Entry<String, Object>> allBindEntries() {
        return binaryExpr.allBindEntries();
    }

    @Override
    public SortedMap<String, Object> localBinds() {
        return binaryExpr.localBinds();
    }

    public boolean equals(Object other) {
        return Objects.equals(binaryExpr, other);
    }

    public int hashCode() {
        return Objects.hashCode(binaryExpr);
    }
}
