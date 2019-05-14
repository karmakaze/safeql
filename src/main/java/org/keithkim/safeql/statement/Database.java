package org.keithkim.safeql.statement;

import com.zaxxer.hikari.HikariDataSource;
import lombok.EqualsAndHashCode;
import org.jdbi.v3.core.Jdbi;
import org.jdbi.v3.sqlobject.SqlObjectPlugin;

import java.util.Optional;

@EqualsAndHashCode
public class Database {
    public final HikariDataSource ds;
    public final Jdbi jdbi;

    public Database(String jdbcUrl, String username, String password) {
        this(jdbcUrl, username, password, null);
    }
    public Database(String jdbcUrl, String username, String password, String driverClassName) {
        ds = new HikariDataSource();
        ds.setJdbcUrl(jdbcUrl);
        ds.setUsername(username);
        ds.setPassword(password);
        Optional.ofNullable(driverClassName).ifPresent(ds::setDriverClassName);
        jdbi = Jdbi.create(ds);
        jdbi.installPlugin(new SqlObjectPlugin());
    }
}
