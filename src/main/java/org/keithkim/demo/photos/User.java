package org.keithkim.demo.photos;

import org.keithkim.typeql.Col;
import org.keithkim.typeql.Entity;
import org.keithkim.typeql.Table;

public class User extends Entity<Long> {
    public final Col<User, Long> idCol;
    public final Col<User, String> nameCol;

    public User(Table<User> table) {
        super("user");
        idCol = new Col<>(User.class, table.alias, "id");
        nameCol = new Col<>(User.class, table.alias, "name");
    }

    public static UserJoinPhoto join(Class<Photo> photoClass) {
        return new UserJoinPhoto(new Table<>(User.class), new Table<>(Photo.class));
    }
}
