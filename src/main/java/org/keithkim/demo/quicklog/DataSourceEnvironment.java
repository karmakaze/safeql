package org.keithkim.demo.quicklog;

import org.keithkim.safeql.statement.Database;
import org.keithkim.safeql.statement.Registry;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class DataSourceEnvironment {
    public static void setup() {
        String jdbcUrl = System.getProperty("JDBC_URL");
        String dbUser = System.getProperty("DB_USER");
        String dbPassword = System.getProperty("DB_PASSWORD");
        String driverClassName = System.getProperty("DB_DRIVER_CLASS_NAME");
        Database db = new Database(jdbcUrl, dbUser, dbPassword, driverClassName);
        Registry.registerDefault(db);
    }

    static {
        try(InputStream inputStream = DataSourceEnvironment.class.getResourceAsStream("/application.properties")) {
            Properties properties = new Properties();
            properties.load(inputStream);
            properties.forEach((key, value) -> {
                System.setProperty((String) key, (String) value);
            });
        } catch (IOException ioe) {
            System.out.println("Something went wrong while reading properties from `application.properties`");
        }
    }
}
