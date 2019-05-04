package org.keithkim.safeql.sql;

import java.util.Optional;

public interface SqlAlias<T> {
    Optional<String> alias();
}
