package org.keithkim.demo.quicklog;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.Database;

import static org.junit.jupiter.api.Assertions.*;

class AccountsTest {
    @Test
    public void mainTest() {
        String jdbcUrl = System.getenv("JDBC_URL");
        String dbUser = System.getenv("DB_USER");
        String dbPassword = System.getenv("DB_PASSWORD");
        Database db = new Database(jdbcUrl, dbUser, dbPassword);
        Accounts accounts = new Accounts(db);
        accounts = accounts.where("id >= 1000");
        accounts.loadProjects();
        for (Account account : accounts) {
            System.out.println(account);
        }
        assertNotNull(accounts);
        assertFalse(accounts.isEmpty());
        Account account = accounts.iterator().next();

        assertNotNull(account);
        assertNotNull(account.projects);
        assertFalse(account.projects.isEmpty());
    }
}
