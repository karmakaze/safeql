package org.keithkim.safeql.statement;

import org.jdbi.v3.core.result.ResultIterable;
import org.keithkim.safeql.schema.Entity;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ListQueryStatement<E extends Entity> extends QueryStatement<E> {
    default List<E> list() {
        return QueryStatement.query(this, ResultIterable::list);
    }

    default CompletableFuture<List<E>> listAsync() {
        return CompletableFuture.supplyAsync(this::list);
    }
}
