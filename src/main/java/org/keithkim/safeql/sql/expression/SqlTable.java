package org.keithkim.safeql.sql.expression;

import com.google.common.base.Joiner;
import org.keithkim.safeql.template.Expr;
import org.keithkim.safeql.util.System;

import java.util.*;

import static java.util.Collections.singletonList;
import static org.keithkim.safeql.sql.expression.Helpers.group;

public class SqlTable<E extends SqlEntity> extends Expr<E> {
    public final Class<E> entityClass;
    private final String alias;

    public SqlTable(Class<E> entityClass) {
        this(entityClass, entityClass.getSimpleName().toLowerCase());
    }

    public SqlTable(Class<E> tableClass, String tableExpr) {
        this(tableClass, tableExpr, null);
    }

    public SqlTable(Class<E> tableClass, String tableExpr, String alias) {
        super(tableExpr);
        this.entityClass = tableClass;
        this.alias = alias;
    }

    public Expr<E> resolve(Map<String, ?> params) {
        if (alias != null) {
            return Expr.expr(group(super.toString()) +" "+ alias);
        }
        return Expr.expr(group(super.toString()));
    }

    public String aliasOrTable() {
        return alias().orElse(super.toString());
    }

    public Optional<String> alias() {
        return Optional.ofNullable(alias);
    }

    public <T> SqlColumn<T> sqlColumn(String columnName) {
        return new SqlColumn<>(columnName);
    }

    public List<E> all() {
        return where(null);
    }

    public List<E> where(String criteria) {
        final String whereCriteria;
        if (criteria != null && !criteria.isEmpty()) {
            whereCriteria = " WHERE " + criteria;
        } else {
            whereCriteria = "";
        }
        return Registry.using(singletonList(this), handle -> {
            return handle.createQuery("SELECT * FROM " + this.toString() + whereCriteria)
                    .mapTo(entityClass)
                    .list();
        });
    }

    public class SqlColumn<T> extends Expr<T> {
        public final String columnName;
        private final String alias;

        public SqlColumn(String columnName) {
            this(columnName, null);
        }

        public SqlColumn(String columnName, String alias) {
            super(columnName);
            this.columnName = columnName;
            this.alias = alias;
        }

        public Expr<T> resolve(Map<String, ?> params) {
            List<String> name = new ArrayList<>(2);
            name.add(SqlTable.this.aliasOrTable());
            name.add(aliasOrColumn());
            return Expr.expr(group(Joiner.on(".").join(name)));
        }

        public String selectTerm(Map<String, ?> params) {
            List<String> name = new ArrayList<>(2);
            // TODO need a better way of identifying literal or other non-columns
            if (SqlTable.this.entityClass != System.Table.none.entityClass) {
                name.add(SqlTable.this.aliasOrTable());
            }
            name.add(super.toString());
            String dotName = Joiner.on(".").join(name);
            return alias == null ? dotName : dotName +" "+ alias;
        }

        public String aliasOrColumn() {
            return alias().orElse(super.toString());
        }

        public Optional<String> alias() {
            return Optional.ofNullable(alias);
        }
    }
}
