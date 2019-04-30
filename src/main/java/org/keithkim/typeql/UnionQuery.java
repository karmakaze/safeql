package org.keithkim.typeql;

import com.google.common.base.Joiner;

public class UnionQuery implements SqlExpr {
    private final Type type;
    private final SelectQuery[] selectQueries;

    public UnionQuery(SelectQuery... selectQueries) {
        this(Type.UNION, selectQueries);
    }

    public UnionQuery(Type type, SelectQuery... selectQueries) {
        this.type = type;
        this.selectQueries = selectQueries;
    }

    public String sql() {
        String joiner = String.format(" %s ", type.name().replace("_", " "));
        return Joiner.on(joiner).join(selectQueries);
    }

    public String toString() {
        return sql();
    }

    public enum Type {
        UNION,
        UNION_DISTINCT,
        UNION_ALL,
    }
}
