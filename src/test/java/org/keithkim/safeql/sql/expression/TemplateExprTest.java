package org.keithkim.safeql.sql.expression;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.keithkim.safeql.sql.expression.Expr;
import org.keithkim.safeql.sql.expression.TemplateExpr;

class TemplateExprTest {

    @Test
    void withBindings_resolve_should() {
        TemplateExpr<String> templateExpr = new TemplateExpr<>("Hello :name :name_1 :name12 :name12_3");
        System.out.println(templateExpr);

        Expr<String> expr = templateExpr.resolve(ImmutableMap.of("name", "Keith", "name12", "twelve"));
        System.out.println(expr);
    }
}
