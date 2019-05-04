package org.keithkim.safeql;

import org.jdbi.v3.core.Handle;
import org.jdbi.v3.core.mapper.JoinRowMapper;
import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.demo.Database;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class Registry {
    private static Database defaultDb = null;
    private static final Map<Class<? extends SqlEntity>, Database> entityDatabase = new HashMap<>();

    public static void registerDefault(Database db) {
        defaultDb = db;
    }

    public static void register(Class<? extends SqlEntity> entityClass, Database db) {
        entityDatabase.put(entityClass, db);
    }

    public static <T> T using(List<SqlTable<? extends SqlEntity>> tables, Function<Handle, T> handleQuery) {
        if (tables == null || tables.isEmpty()) {
            throw new RuntimeException("no tables");
        }
        Database db = null;
        for (SqlTable<? extends SqlEntity> table : tables) {
            Database db1 = entityDatabase.get(table.entityClass);
            if (db1 == null) {
                db1 = defaultDb;
                if (db1 == null) {
                    throw new RuntimeException(table.entityClass.getName() +" not in Registry");
                }
            }
            if (db == null) {
                db = db1;
            } else if (db1 != db) {
                throw new RuntimeException("using entities of different databases");
            }
        }
        return db.jdbi.withHandle(handle -> {
            for (SqlTable<? extends SqlEntity> table : tables) {
                if (table.alias().isPresent()) {
                    handle.registerRowMapper(ConstructorMapper.factory(table.entityClass, table.alias().get()+"_"));
                } else {
                    handle.registerRowMapper(ConstructorMapper.factory(table.entityClass));
                }
            }
            return handleQuery.apply(handle);
        });
    }
}
