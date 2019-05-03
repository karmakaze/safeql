package org.keithkim.typeql;

import com.google.common.base.Joiner;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class UnionQuery extends SqlExpr {
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

    public void bind(String name, Object value) {
        if (missingParams().contains(name)) {
            params().put(name, Optional.of(value));
        }
        for (SelectQuery q : selectQueries) {
            q.bind(name, value);
        }
    }

    public Map<String, Optional<?>> params() {
        Map<String, Optional<?>> params = new HashMap<>();
        for (SelectQuery q : selectQueries) {
            params.putAll(q.params());
        }
        return params;
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
