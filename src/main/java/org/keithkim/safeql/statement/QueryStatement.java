package org.keithkim.safeql.statement;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.jdbi.v3.core.result.ResultIterable;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;
import org.slf4j.Logger;

import java.util.List;
import java.util.function.Function;

public interface QueryStatement<E extends Entity> extends Statement {
    Logger log = org.slf4j.LoggerFactory.getLogger(QueryStatement.class);

    Class<E> entityClass();
    List<Table<? extends Entity>> tables();

    static <E extends Entity, T> T query(QueryStatement<E> stmt, Function<ResultIterable<E>, T> fn) {
        String sql = stmt.sql();
        log.info("SQL: {}", sql);

        List<Table<? extends Entity>> tables = stmt.tables();

        return TableDbRegistry.using(tables, handle -> {
            for (Table<?> table : tables) {
                if (table.alias().isPresent()) {
                    handle.registerRowMapper(ConstructorMapper.factory(table.entityClass, table.alias().get() + "_"));
                } else {
                    handle.registerRowMapper(ConstructorMapper.factory(table.entityClass));
                }
            }
            ResultIterable<E> resultIterable = handle.createQuery(sql).mapTo(stmt.entityClass());

            return fn.apply(resultIterable);
        });
    }
}
