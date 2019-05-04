package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;

public class CollectionItem extends SqlEntity<Long> {
    public final SqlColumn<CollectionItem, Long> collectionIdCol;
    public final SqlColumn<CollectionItem, Long> photoIdCol;

    public CollectionItem() {
        super("collection_item");
        collectionIdCol = new SqlColumn<>(this, "collection_id");
        photoIdCol = new SqlColumn<>(this, "photo_id");
    }

    public static Class<UserJoinPhoto> join(Class<Photo> photoClass) {
        return UserJoinPhoto.class;
    }
}
