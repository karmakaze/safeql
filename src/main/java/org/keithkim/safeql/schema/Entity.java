package org.keithkim.safeql.schema;

import lombok.EqualsAndHashCode;

import java.util.Map;

@EqualsAndHashCode
public abstract class Entity<PkType> {
    public abstract Entity<PkType> withId(PkType id);
    public abstract Map<String, ?> attributes();
}
