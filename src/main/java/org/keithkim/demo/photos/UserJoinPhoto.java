package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlTable;

public class UserJoinPhoto extends UserJoinPhotoOn {
    public UserJoinPhoto(SqlTable<User> userAlias, SqlTable<Photo> photoAlias) {
        super(userAlias, new User(userAlias).idCol, photoAlias, new Photo(photoAlias).userIdCol);
    }

    public <T> UserJoinPhotoOn on(SqlColumn<User, T> userCol, SqlColumn<Photo, T> photoCol) {
        equates.clear();
        equates.add(new Equate(userCol, photoCol));
        return this;
    }
}
