package org.keithkim.demo.photos;

import org.keithkim.typeql.Col;
import org.keithkim.typeql.Entity;
import org.keithkim.typeql.Table;

public class Photo extends Entity<Long> {
    public final Col<Photo, Long> idCol;
    public final Col<Photo, Long> userIdCol;

    public static UserJoinPhoto join(Class<User> userClass) {
        return new UserJoinPhoto(new Table<>(User.class), new Table<>(Photo.class));
    }

    public Photo(Table<Photo> table) {
        super("photo");
        idCol = new Col<>(Photo.class, table.alias, "id");
        userIdCol = new Col<>(Photo.class, table.alias, "user_id");
    }
}
