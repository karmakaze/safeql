package org.keithkim.safeql.template;

import java.util.Map;
import java.util.Optional;

import static java.util.Collections.emptyMap;

public class Expr<T> {
    final String string;

    public static <T> Expr<T> expr(String string) {
        return new Expr<>(string);
    }

    public Expr(String string) {
        this.string = string;
    }

    public Map<String, Optional<?>> params() {
        return emptyMap();
    }

    public Expr<T> resolve(Map<String, ?> params) {
        return this;
    }

    public String toString() {
        return string;
    }
}
