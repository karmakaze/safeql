package org.keithkim.safeql.statement;

import org.slf4j.Logger;

import java.util.concurrent.CompletableFuture;

import static java.util.Collections.emptyList;

public interface UpdateStatement extends Statement {
    Logger log = org.slf4j.LoggerFactory.getLogger(UpdateStatement.class);

    default int execute() {
        String sql = sql();
        log.info("SQL: {}", sql);

        return TableDbRegistry.using(emptyList(), handle -> handle.createUpdate(sql).execute());
    }

    default CompletableFuture<Integer> executeAsync() {
        return CompletableFuture.supplyAsync(this::execute);
    }
}
