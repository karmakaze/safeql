package org.keithkim.safeql.type;

import lombok.EqualsAndHashCode;
import org.jdbi.v3.core.Handle;
import org.jdbi.v3.core.mapper.JoinRowMapper;
import org.keithkim.safeql.schema.Entity;

@EqualsAndHashCode
public class JoinRows<L extends Entity, R extends Entity> {
    private final Class<L> leftClass;
    private final Class<R> rightClass;

    public JoinRows(Class<L> leftClass, Class<R> rightClass) {
        this.leftClass = leftClass;
        this.rightClass = rightClass;
    }

    public Handle registerTypes(Handle handle) {
        return handle.registerRowMapper(JoinRowMapper.forTypes(leftClass, rightClass));
    }
}
