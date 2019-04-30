package org.keithkim.typeql;

public class Table<T extends Entity> extends TableExpr {
    public final Class<T> tableClass;

    public Table(Class<T> tableClass) {
        this(tableClass, tableClass.getName().toLowerCase());
    }

    public Table(Class<T> tableClass, String alias) {
        super(tableClass.getSimpleName().toLowerCase(), alias);
        this.tableClass = tableClass;
    }
}
