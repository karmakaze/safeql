package org.keithkim.safeql.statement;

import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;

import java.util.List;

import static java.util.Arrays.asList;
import static org.keithkim.safeql.util.StringHelpers.snakeCase;

public class RawQueryStatement<E extends Entity> implements SingleQueryStatement<E>, ListQueryStatement<E> {
    private final String sql;
    private final Class<E> entityClass;
    List<Table<? extends Entity>> tables;

    public RawQueryStatement(String sql, Class<E> entityClass) {
        this(sql, entityClass, new Table<>(entityClass, snakeCase(entityClass.getSimpleName())));
    }

    public RawQueryStatement(String sql, Class<E> entityClass, String tableName) {
        this(sql, entityClass, new Table<>(entityClass, tableName));
    }

    public RawQueryStatement(String sql, Class<E> entityClass, Table<? extends Entity>... tables) {
        this.sql = sql;
        this.entityClass = entityClass;
        this.tables = asList(tables);
    }

    public String sql() {
        return sql;
    }

    public Class<E> entityClass() {
        return entityClass;
    }

    public List<Table<? extends Entity>> tables() {
        return tables;
    }
}
