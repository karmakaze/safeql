package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

public class Photo extends SqlEntity<Long> {
    public final SqlColumn<Photo, Long> idCol;
    public final SqlColumn<Photo, Long> userIdCol;

    public static UserJoinPhoto join(Class<User> userClass) {
        return new UserJoinPhoto(new SqlTable<>(User.class), new SqlTable<>(Photo.class));
    }

    public Photo(SqlTable<Photo> table) {
        super("photo");
        idCol = new SqlColumn<>(this, "id");
        userIdCol = new SqlColumn<>(this, "user_id");
    }

    public SqlColumn<Photo, Long> userIdCol() {
        return new SqlColumn<>(this, "user_id");
    }
}
