package org.keithkim.demo.photos;

import org.keithkim.typeql.*;

public class UserJoinPhotoOn<T> extends Join<User, Photo> implements SqlExpression {
    static UserJoinPhotoOn natural(Table<User> userAlias, Table<Photo> photoAlias) {
        return new UserJoinPhotoOn(userAlias, new User(userAlias).idCol, photoAlias, new Photo(photoAlias).userIdCol);
    }

    UserJoinPhotoOn(Table<User> userAlias, Col<User, T> userCol, Table<Photo> photoAlias, Col<Photo, T> photoCol) {
        super(userAlias, userCol, photoAlias, photoCol);
    }

    public UserJoinPhotoOn where(Cond2 cond) {
        andWhere.add(cond);
        return this;
    }

    public static class Where {
    }
}
