package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;

public class CollectionItem extends SqlEntity<Long> {
    public class CollectionId extends SqlColumn<CollectionItem, Long>{
        public CollectionId() {
            super(CollectionItem.this, "collection_id");
        }
    }
    public class PhotoId extends SqlColumn<CollectionItem, Long>{
        public PhotoId() {
            super(CollectionItem.this, "user_id");
        }
    }

    public final CollectionId collectionIdCol = new CollectionId();
    public final PhotoId photouserIdCol = new PhotoId();

    public CollectionItem() {
        super("collection_item");
    }

    public static Class<UserJoinPhoto> join(Class<Photo> photoClass) {
        return UserJoinPhoto.class;
    }
}
