package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;

public class Collection extends SqlEntity<Long> {
    public final SqlColumn<Collection, Long> idCol;
    public final SqlColumn<Collection, Long> userIdCol;

    public Collection() {
        super("collection");
        idCol = new SqlColumn<>(this, "id");
        userIdCol = new SqlColumn<>(this, "user_id");

    }

    public static Class<UserJoinPhoto> join(Class<Photo> photoClass) {
        return UserJoinPhoto.class;
    }
}
