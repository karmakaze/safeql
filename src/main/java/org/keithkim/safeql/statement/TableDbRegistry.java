package org.keithkim.safeql.statement;

import org.jdbi.v3.core.Handle;
import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class TableDbRegistry {
    private static volatile Database defaultDb = null;
    private static final Map<Class<? extends Entity>, Database> entityDatabase = new HashMap<>();

    public static void registerDefault(Database db) {
        defaultDb = db;
    }

    public static void register(Class<? extends Entity> entityClass, Database db) {
        entityDatabase.put(entityClass, db);
    }

    public static <T> T using(List<Table<? extends Entity>> tables, Function<Handle, T> handleQuery) {
        Database db = null;
        for (Table<? extends Entity> table : tables) {
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
        if (db == null) {
            db = defaultDb;
            if (db == null) {
                throw new RuntimeException("no default db in Registry");
            }
        }
        return db.jdbi.withHandle(handle -> {
            for (Table<? extends Entity> table : tables) {
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
