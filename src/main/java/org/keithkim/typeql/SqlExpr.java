package org.keithkim.typeql;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public abstract class SqlExpr implements SqlExpression {
    private final Map<String, Optional<?>> params = new HashMap<>();

    public void bind(String name, Object value) {
        if (missingParams().contains(name)) {
            params.put(name, Optional.of(value));
        }
    }

    public Map<String, Optional<?>> params() {
        return params;
    }
}
