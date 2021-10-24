package org.keithkim.safeql.schema;

import org.keithkim.safeql.predicate.Predicate;
import org.keithkim.safeql.statement.TableDbRegistry;

import java.util.ArrayList;
import java.util.List;

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
        List<E> accounts = TableDbRegistry.using(singletonList(table), handle -> {
            String whereClause = "";
            if (cond != null) {
                whereClause = " WHERE " + cond.sql();
            }
            return handle.createQuery(String.format("SELECT * FROM %s %s", table.sqlNoAlias(), whereClause))
                    .mapTo(table.entityClass)
                    .list();
        });
        return newEntities(accounts);
    }
}
