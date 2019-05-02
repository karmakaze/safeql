package org.keithkim.typeql;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.toSet;

public interface SqlExpression {
    String sql();

    default void bind(String name, Object value) {
    }

    default Map<String, Optional<?>> params() {
        return emptyMap();
    }

    default Set<String> missingParams() {
        return params().entrySet().stream()
                .filter(p -> !p.getValue().isPresent())
                .map(Map.Entry::getKey)
                .collect(toSet());
    }
}
