package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

public class Photo extends SqlEntity<Long> {
    public class Id extends SqlColumn<Photo, Long>{
        public Id() {
            super(Photo.this, "id");
        }
    }
    public class UserId extends SqlColumn<Photo, Long>{
        public UserId() {
            super(Photo.this, "user_id");
        }
    }

    public final Id idCol = new Id();
    public final UserId userIdCol = new UserId();

    public static UserJoinPhoto join(Class<User> userClass) {
        return new UserJoinPhoto(new SqlTable<>(User.class), new SqlTable<>(Photo.class));
    }

    public Photo(SqlTable<Photo> table) {
        super("photo");
    }

    public SqlColumn<Photo, Long> userIdCol() {
        return new SqlColumn<>(this, "user_id");
    }
}
