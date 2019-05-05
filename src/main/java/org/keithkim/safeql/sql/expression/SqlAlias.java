package org.keithkim.safeql.sql.expression;

import java.util.Optional;

public interface SqlAlias<T> {
    Optional<String> alias();
}
