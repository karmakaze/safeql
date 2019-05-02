package org.keithkim.typeql.template;

import java.util.Map;

public class Expr<T> {
    final String string;

    public Expr(String string) {
        this.string = string;
    }

    Expr<T> render(Map<String, ?> params) {
        return this;
    }

    public String toString() {
        return string;
    }
}
