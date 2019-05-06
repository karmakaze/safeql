package org.keithkim.safeql.query;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.type.Rows;

import java.util.regex.Pattern;

@EqualsAndHashCode
public class RawSelect<E extends Entity> extends Expr<Rows<E>> {
    private static final Pattern varPattern = Pattern.compile(":[A-Za-z][A-Za-z0-9_]*");
    private final Class<E> entityClass;

    public RawSelect(Class<E> entityClass, String sql) {
        super(sql);
        this.entityClass = entityClass;
    }
}
