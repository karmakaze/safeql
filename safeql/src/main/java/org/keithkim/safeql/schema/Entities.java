package org.keithkim.safeql.schema;

import org.jdbi.v3.core.result.ResultIterable;
import org.jdbi.v3.core.statement.Query;
import org.keithkim.safeql.predicate.Predicate;
import org.keithkim.safeql.statement.TableDbRegistry;

import java.util.*;

import static java.util.Collections.singletonList;

public abstract class Entities<EE extends Entities, E extends Entity> extends ArrayList<E> {
    protected final Table<E> table;

    public Entities(Table<E> table) {
        super();
        this.table = table;
    }

    public Entities(Table<E> table, List<E> entities) {
        super(entities);
        this.table = table;
    }

    public abstract EE newEntities(List<E> entities);

    public EE where(Predicate cond) {
        List<E> entities = TableDbRegistry.using(singletonList(table), handle -> {
            String whereClause = "";
            if (cond != null) {
                whereClause = " WHERE " + cond.sql();
            }
            Query query = handle.createQuery(String.format("SELECT * FROM %s %s", table.sqlNoAlias(), whereClause));
            for (Map.Entry<String, ?> me : cond.allBindEntries()) {
                String name = me.getKey();
                Object arg = me.getValue();
                if (arg instanceof Collection) {
                    query = query.bindList(name, arg);
                } else {
                    query = query.bind(name, arg);
                }
            }

            return query.mapTo(table.entityClass).list();
        });
        return newEntities(entities);
    }
}
