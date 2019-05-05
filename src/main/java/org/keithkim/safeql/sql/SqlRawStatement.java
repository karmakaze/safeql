package org.keithkim.safeql.sql;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.Collections.emptyList;

public class SqlRawStatement<T> implements SqlStatement<T> {
    private final Class<T> entityClass;
    private final String statement;

    public SqlRawStatement(Class<T> entityClass, String statement) {
        this.entityClass = entityClass;
        this.statement = statement;
    }

    @Override
    public List<T> queryList() {
        return Registry.using(emptyList(), handle -> {
            return handle.createQuery(statement)
                    .registerRowMapper(ConstructorMapper.factory(entityClass))
                    .mapTo(entityClass)
                    .list();
        });
    }

    @Override
    public CompletableFuture<List<T>> queryListAsync() {
        return CompletableFuture.supplyAsync(() -> {
            return queryList();
        });
    }

    @Override
    public int execute() {
        return Registry.using(emptyList(), handle -> {
            return handle.createUpdate(statement)
                    .execute();
        });
    }

    @Override
    public CompletableFuture<Integer> executeAsync() {
        return CompletableFuture.supplyAsync(this::execute);
    }
}
