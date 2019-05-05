package org.keithkim.safeql.sql.statement;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.jdbi.v3.core.result.ResultIterable;
import org.keithkim.safeql.sql.expression.Registry;
import org.keithkim.safeql.sql.expression.SqlEntity;
import org.keithkim.safeql.sql.expression.SqlTable;
import org.slf4j.Logger;

import java.util.List;
import java.util.function.Function;

public interface QueryStatement<E extends SqlEntity> extends Statement {
    Logger log = org.slf4j.LoggerFactory.getLogger(QueryStatement.class);

    Class<E> entityClass();
    List<SqlTable<? extends SqlEntity>> tables();

    static <E extends SqlEntity, T> T query(QueryStatement<E> stmt, Function<ResultIterable<E>, T> fn) {
        String sql = stmt.sql();
        log.info("SQL: {}", sql);

        List<SqlTable<? extends SqlEntity>> tables = stmt.tables();

        return Registry.using(tables, handle -> {
            for (SqlTable<?> table : tables) {
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
