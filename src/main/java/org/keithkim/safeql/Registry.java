package org.keithkim.safeql;

import org.jdbi.v3.core.Handle;
import org.jdbi.v3.core.mapper.JoinRow;
import org.jdbi.v3.core.mapper.JoinRowMapper;
import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.demo.Database;
import org.keithkim.demo.quicklog.Account;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class Registry {
    public static final Map<Class<? extends SqlEntity>, Database> entityDatabase = new HashMap<>();

    public static void register(Class<? extends SqlEntity> entityClass, Database db) {
        entityDatabase.put(entityClass, db);
    }

    public static <T> T using(List<SqlTable<SqlEntity>> tables, Function<Handle, T> handleQuery) {
        if (tables == null || tables.isEmpty()) {
            throw new RuntimeException("no tables");
        }
        Database db = null;
        for (SqlTable<SqlEntity> table : tables) {
            Database db1 = entityDatabase.get(table.entityClass);
            if (db1 == null) {
                throw new RuntimeException("not registered: "+ table.entityClass.getName());
            }
            if (db == null) {
                db = db1;
            } else if (db1 != db) {
                throw new RuntimeException("using entities of different databases");
            }
        }
        return db.jdbi.withHandle(handle -> {
            for (SqlTable<SqlEntity> table : tables) {
                if (table.alias().isPresent()) {
                    handle.registerRowMapper(ConstructorMapper.factory(table.entityClass, table.alias().get()+"_"));
                } else {
                    handle.registerRowMapper(ConstructorMapper.factory(table.entityClass));
                }
            }
            if (tables.size() == 2) {
                handle.registerRowMapper(JoinRowMapper.forTypes(tables.get(0).entityClass, tables.get(1).entityClass));
            }
            return handleQuery.apply(handle);
        });
    }
}
