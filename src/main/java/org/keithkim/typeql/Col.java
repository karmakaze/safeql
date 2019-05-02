package org.keithkim.typeql;

public class Col<E extends Entity, T> extends SqlExpr implements Field<T> {
    public final Class<E> entityClass;
    public final String tableAlias;
    public final String colName;

    public Col(Class<E> entityClass, String colName) {
        this(entityClass, entityClass.getName().toLowerCase(), colName);
    }
    public Col(Class<E> entityClass, String tableAlias, String colName) {
        this.entityClass = entityClass;
        this.tableAlias = tableAlias;
        this.colName = colName;
    }

    public String fieldName() {
        return tableAlias +"."+ colName;
    }

    public String sql() {
        return fieldName();
    }

    public String toString() {
        return sql();
    }
}
