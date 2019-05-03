package org.keithkim.demo.photos;

import org.keithkim.safeql.Col;
import org.keithkim.safeql.Entity;

public class Collection extends Entity<Long> {
    public final Col<Collection, Long> idCol;
    public final Col<Collection, Long> userIdCol;

    public Collection() {
        super("collection");
        idCol = new Col<>(Collection.class, "id");
        userIdCol = new Col<>(Collection.class, "user_id");

    }

    public static Class<UserJoinPhoto> join(Class<Photo> photoClass) {
        return UserJoinPhoto.class;
    }
}
