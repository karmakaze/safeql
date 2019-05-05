package org.keithkim.safeql.sql.statement;

import org.keithkim.safeql.sql.expression.SqlEntity;
import org.keithkim.safeql.sql.expression.SqlTable;

import java.util.List;

import static java.util.Arrays.asList;
import static org.keithkim.safeql.util.StringHelpers.snakeCase;

public class RawQueryStatement<E extends SqlEntity> implements SingleQueryStatement<E>, ListQueryStatement<E> {
    private final String sql;
    private final Class<E> entityClass;
    List<SqlTable<? extends SqlEntity>> tables;

    public RawQueryStatement(String sql, Class<E> entityClass) {
        this(sql, entityClass, new SqlTable<>(entityClass, snakeCase(entityClass.getSimpleName())));
    }

    public RawQueryStatement(String sql, Class<E> entityClass, String tableName) {
        this(sql, entityClass, new SqlTable<>(entityClass, tableName));
    }

    public RawQueryStatement(String sql, Class<E> entityClass, SqlTable<? extends SqlEntity>... tables) {
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

    public List<SqlTable<? extends SqlEntity>> tables() {
        return tables;
    }
}
