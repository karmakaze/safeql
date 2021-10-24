package org.keithkim.safeql.schema;

import lombok.EqualsAndHashCode;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

@EqualsAndHashCode
public abstract class Entity<PkType> {
    protected Map<Class<?>, LinkedHashMap<?, ? extends Entity>> childrenByClass = new HashMap<>();

    public abstract PkType id();

    public void addChild(Entity childEntity) {
        LinkedHashMap<?, ? extends Entity> children = childrenByClass.get(childEntity.getClass());
        if (children == null) {
            children = new LinkedHashMap<>();
            childrenByClass.put(childEntity.getClass(), children);
        }
        ((Map<Object, Object>) (Map) childrenByClass).put(childEntity.id(), childEntity);
    }
}
