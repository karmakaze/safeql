package org.keithkim.demo.photos;

import org.keithkim.typeql.Col;
import org.keithkim.typeql.Table;

public class UserJoinPhoto extends UserJoinPhotoOn<Long> {
    public UserJoinPhoto(Table<User> userAlias, Table<Photo> photoAlias) {
        super(userAlias, new User(userAlias).idCol, photoAlias, new Photo(photoAlias).userIdCol);
    }

    public <T> UserJoinPhotoOn on(Col<User, T> userCol, Col<Photo, T> photoCol) {
        equates.clear();
        equates.add(new Equate(userCol, photoCol));
        return this;
    }
}
