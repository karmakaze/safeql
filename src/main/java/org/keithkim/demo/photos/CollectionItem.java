package org.keithkim.demo.photos;

import org.keithkim.safeql.Col;
import org.keithkim.safeql.Entity;

public class CollectionItem extends Entity<Long> {
    public final Col<CollectionItem, Long> collectionIdCol;
    public final Col<CollectionItem, Long> photoIdCol;

    public CollectionItem() {
        super("collection_item");
        collectionIdCol = new Col<>(CollectionItem.class, "collection_id");
        photoIdCol = new Col<>(CollectionItem.class, "photo_id");
    }

    public static Class<UserJoinPhoto> join(Class<Photo> photoClass) {
        return UserJoinPhoto.class;
    }
}
