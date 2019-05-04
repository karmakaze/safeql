package org.keithkim.demo.photos;

import org.keithkim.safeql.Join;
import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlTable;

public class UserJoinPhotoOn extends Join<User, Photo> {
    static UserJoinPhotoOn natural(SqlTable<User> userAlias, SqlTable<Photo> photoAlias) {
        return new UserJoinPhotoOn(userAlias, new User(userAlias).idCol(), photoAlias, new Photo(photoAlias).userIdCol());
    }

    UserJoinPhotoOn(SqlTable<User> userAlias, SqlColumn<User, Long> userCol, SqlTable<Photo> photoAlias, SqlColumn<Photo, Long> photoCol) {
        super(userAlias, userCol, photoAlias, photoCol);
    }

    public UserJoinPhotoOn where(Cond2 cond) {
        andWhere.add(cond);
        return this;
    }

    public static class Where {
    }
}
