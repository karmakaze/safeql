package org.keithkim.safeql.sql.expression;

import org.keithkim.safeql.sql.expression.Expr;

import java.util.Map;

public class TemplateExpr<T> extends Expr<T> {
    public TemplateExpr(String template) {
        super(template);
    }

    public Expr<T> resolve(Map<String, ?> params) {
        String rendered = super.toString();

        for (Map.Entry<String, ?> me : params.entrySet()) {
            String name = me.getKey();
            Object value = me.getValue();
            if (value != null) {
                rendered = rendered.replaceAll(":" + name + "\\b", value.toString());
            }
        }
        return new Expr<>(rendered);
    }
}
