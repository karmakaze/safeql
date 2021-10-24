package org.keithkim.safeql.statement;

import com.zaxxer.hikari.HikariDataSource;
import lombok.EqualsAndHashCode;
import org.jdbi.v3.core.Jdbi;
import org.jdbi.v3.sqlobject.SqlObjectPlugin;

@EqualsAndHashCode
public class Database {
    public final HikariDataSource ds;
    public final Jdbi jdbi;

    public Database(String jdbcUrl, String username, String password) {
        ds = new HikariDataSource();
        ds.setJdbcUrl(jdbcUrl);
        ds.setUsername(username);
        ds.setPassword(password);
        jdbi = Jdbi.create(ds);
        jdbi.installPlugin(new SqlObjectPlugin());
    }
}
