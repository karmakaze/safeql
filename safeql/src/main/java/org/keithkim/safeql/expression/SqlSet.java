package org.keithkim.safeql.expression;

import java.util.Set;

public interface SqlSet<T> extends Sql<Set<T>> {
    String sql();
    boolean isTerm();
}
