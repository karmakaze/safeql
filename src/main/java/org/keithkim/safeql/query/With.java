package org.keithkim.safeql.query;

import com.google.common.base.Joiner;
import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;
import org.keithkim.safeql.type.Rows;

import java.util.List;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.keithkim.safeql.predicate.Predicates.ALL;

@EqualsAndHashCode(callSuper = false)
public class With<E extends Entity> extends Expr<Rows<E>> {
    public enum Type {
        WITH("WITH"),
        WITH_RECURSIVE("WITH RECURSIVE");

        private final String string;

        Type(String string) {
            this.string = string;
        }

        public String sql() {
            return string;
        }
    }

    protected final Type type;
    protected final List<Table<?>> tables;

    public With(Table<?>... tables) {
        this(Type.WITH, tables);
    }

    public With(Type type, Table<?>... tables) {
        super(null);
        this.type = type;
        this.tables = asList(tables);
    }

    public String sql() {
        if (tables.isEmpty()) {
            return "";
        }
        List<Object> tableNameAsTableExprs = tables.stream()
                .map(t -> t.aliasOrTable() +" AS ("+ t.sqlNoAlias() +"\n     )")
                .collect(toList());
        return type.sql() +' '+ Joiner.on(",\n     ").join(tableNameAsTableExprs);
    }
}
