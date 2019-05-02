package org.keithkim.typeql.template;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class TemplateExpr<T> extends Expr<T> {
    public TemplateExpr(String template) {
        super(template);
    }

    Expr<T> render(Map<String, ?> params) {
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

    public static void main(String[] args) {
        TemplateExpr<String> templateExpr = new TemplateExpr<>("Hello :name :name_1 :name12 :name12_3");
        System.out.println(templateExpr);

        Expr<String> expr = templateExpr.render(ImmutableMap.of("name", "Keith", "name12", "twelve"));
        System.out.println(expr);
    }
}
