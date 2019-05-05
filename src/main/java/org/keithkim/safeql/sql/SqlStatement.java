package org.keithkim.safeql.sql;

import java.time.Instant;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public interface SqlStatement<E> {
    default E querySingle() {
        throw new UnsupportedOperationException();
    }
    default CompletableFuture<E> querySingleAsync() {
        return CompletableFuture.supplyAsync(() -> {
            return querySingle();
        });
    }
    default List<E> queryList() {
        throw new UnsupportedOperationException();
    }
    default CompletableFuture<List<E>> queryListAsync() {
        return CompletableFuture.supplyAsync(() -> {
            return queryList();
        });
    }
    default int execute() {
        throw new UnsupportedOperationException();
    }
    default CompletableFuture<Integer> executeAsync() {
        return CompletableFuture.supplyAsync(() -> {
            return execute();
        });
    }
}
