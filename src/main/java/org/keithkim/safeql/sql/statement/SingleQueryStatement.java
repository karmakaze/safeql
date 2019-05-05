package org.keithkim.safeql.sql.statement;

import org.jdbi.v3.core.result.ResultIterable;
import org.keithkim.safeql.sql.expression.SqlEntity;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

public interface SingleQueryStatement<E extends SqlEntity> extends QueryStatement<E> {
    default Optional<E> findFirst() {
        return QueryStatement.query(this, ResultIterable::findFirst);
    }

    default CompletableFuture<Optional<E>> findFirstAsync() {
        return CompletableFuture.supplyAsync(this::findFirst);
    }
}
