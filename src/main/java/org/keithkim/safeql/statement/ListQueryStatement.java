package org.keithkim.safeql.statement;

import org.jdbi.v3.core.result.ResultIterable;
import org.keithkim.moja.monad.Async;
import org.keithkim.moja.monad.Multi;
import org.keithkim.safeql.schema.Entity;

import java.util.List;

public interface ListQueryStatement<E extends Entity> extends QueryStatement<E> {
    default List<E> list() {
        return QueryStatement.query(this, ResultIterable::list);
    }

    default Async<Multi<E>> multiAsync() {
        return Async.async(() -> Multi.of(list()));
    }
}
