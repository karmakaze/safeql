package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;

public class Collection extends SqlEntity<Long> {
    public class Id extends SqlColumn<Collection, Long>{
        public Id() {
            super(Collection.this, "id");
        }
    }
    public class UserId extends SqlColumn<Collection, Long>{
        public UserId() {
            super(Collection.this, "user_id");
        }
    }

    public final Id idCol = new Id();
    public final UserId userIdCol = new UserId();

    public Collection() {
        super("collection");

    }

    public static Class<UserJoinPhoto> join(Class<Photo> photoClass) {
        return UserJoinPhoto.class;
    }
}
