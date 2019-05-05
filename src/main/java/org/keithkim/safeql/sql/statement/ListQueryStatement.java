package org.keithkim.safeql.sql.statement;

import org.jdbi.v3.core.result.ResultIterable;
import org.keithkim.safeql.sql.expression.SqlEntity;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ListQueryStatement<E extends SqlEntity> extends QueryStatement<E> {
    default List<E> list() {
        return QueryStatement.query(this, ResultIterable::list);
    }

    default CompletableFuture<List<E>> listAsync() {
        return CompletableFuture.supplyAsync(this::list);
    }
}
