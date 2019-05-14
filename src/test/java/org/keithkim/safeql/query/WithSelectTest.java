package org.keithkim.safeql.query;

import com.google.common.base.Joiner;
import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Account;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.schema.Table;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.*;

class WithSelectTest {
    @Test
    void withZeroCommon_sql_justHasTheSelect() {
        With<Account> withAccount = new With<>();
        WithSelect<Account> select = new WithSelect<>(withAccount, new Table<>(Account.class, "account"));

        assertEquals("SELECT * FROM account", select.sql());
    }

    @Test
    void withOneCommon_sql_hasSingleWithAndSelect() {
        Table<Account> accountTable = new Table<>(Account.class, "SELECT * FROM account WHERE MOD(id, 3) = 1", "a");
        With<Account> withAccount = new With<>(accountTable);
        WithSelect<Account> select = new WithSelect<>(withAccount, new Table<>(Account.class, "a"));

        assertEquals("WITH a AS (SELECT * FROM account WHERE MOD(id, 3) = 1\n     )\nSELECT * FROM a", select.sql());
    }

    @Test
    void withTwoCommon_sql_hasBothWithAndSelect() {
        Table<Account> fizzTable = new Table<>(Account.class, "SELECT * FROM account WHERE MOD(id, 3) = 0", "fizz");
        Table<Project> buzzTable = new Table<>(Project.class, "SELECT * FROM project WHERE MOD(id, 5) = 0", "buzz");
        With<Account> withAccount = new With<>(fizzTable, buzzTable);
        WithSelect<Account> select = new WithSelect<>(withAccount, new Table<>(Account.class, "fizz f JOIN buzz b ON MOD(f.id / 3, 7)  == MOD(b.id / 5, 7)"));

        String expected = Joiner.on("\n").join(asList(
                "WITH fizz AS (SELECT * FROM account WHERE MOD(id, 3) = 0",
                "     ),",
                "     buzz AS (SELECT * FROM project WHERE MOD(id, 5) = 0",
                "     )",
                "SELECT * FROM fizz f JOIN buzz b ON MOD(f.id / 3, 7)  == MOD(b.id / 5, 7)"));
        assertEquals(expected, select.sql());
    }
}
