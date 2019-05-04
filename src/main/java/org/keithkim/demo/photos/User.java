package org.keithkim.demo.photos;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

public class User extends SqlEntity<Long> {
    public class Id extends SqlColumn<User, Long>{
        public Id() {
            super(User.this, "id");
        }
    }
    public class Name extends SqlColumn<User, String>{
        public Name() {
            super(User.this, "name");
        }
    }

    public final SqlColumn<User, Long> idCol;
    public final SqlColumn<User, String> nameCol;

    public User(SqlTable<User> table) {
        super("user");
        idCol = new SqlColumn<>(this, "id");
        nameCol = new SqlColumn<>(this, "name");
    }

    public SqlColumn<User, Long> idCol() {
        return new SqlColumn<>(this, "id");
    }

    public static UserJoinPhoto join(Class<Photo> photoClass) {
        return new UserJoinPhoto(new SqlTable<>(User.class), new SqlTable<>(Photo.class));
    }
}
