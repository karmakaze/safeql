package org.keithkim.safeql.template;

import com.google.common.base.Joiner;

import java.util.List;
import java.util.Map;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.keithkim.safeql.template.Helpers.group;

public class NAryExpr<T> extends Expr<T> {
    private final Expr<T> identity;
    private final Expr<T>[] exprs;

    public NAryExpr(String operator, Expr<T> identity, Expr<T>... exprs) {
        super(operator);
        this.identity = identity;
        this.exprs = exprs;
    }

    public Expr<T> resolve(Map<String, ?> params) {
        if (exprs.length == 0) {
            return identity;
        } else if (exprs.length == 1) {
            return exprs[0].resolve(params);
        }
        String operator = super.string;
        List<String> terms = asList(exprs).stream().map(expr -> group(expr.resolve(params).toString())).collect(toList());
        if (operator.endsWith("()")) {
            String arguments = Joiner.on(", ").join(terms);
            return new Expr<>(operator.substring(0, operator.length() - 1) + arguments +")");
        } else {
            return new Expr<>(Joiner.on(" "+operator+" ").join(terms));
        }
    }
}
