package org.keithkim.safeql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Account;
import org.keithkim.demo.quicklog.AccountAndProject;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.Join.Equate;
import org.keithkim.safeql.Join.Type;
import org.keithkim.safeql.sql.SqlJoinRows;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class JoinTest {
    Account.Table accountTable = new Account.Table("account", "a");
    Project.Table projectTable = new Project.Table("project", "p");

    @Test
    void defaultJoin() {
        Join joinQuery = new Join(accountTable, accountTable.idCol, projectTable, projectTable.accountIdCol);
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void defaultJoinEquate() {
        Join joinQuery = new Join(accountTable, projectTable, new Equate(accountTable.idCol, projectTable.accountIdCol));
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void fullJoinEquate() {
        Join joinQuery = new Join(Type.FULL_JOIN, accountTable, projectTable, new Equate(accountTable.idCol, projectTable.accountIdCol));
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a FULL JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void leftJoinEquate2() {
        Join joinQuery = new Join(Type.LEFT_JOIN, accountTable, projectTable, new Equate(accountTable.idCol, projectTable.accountIdCol), new Equate(accountTable.idCol, projectTable.accountIdCol));
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a LEFT JOIN project p ON a.id = p.account_id AND a.id = p.account_id", resolved.toString());
    }

    @Test
    void join() {
        Join joinQuery = new Join(Type.JOIN, accountTable, accountTable.idCol, projectTable, projectTable.accountIdCol);
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void leftJoin() {
        Join joinQuery = new Join(Type.LEFT_JOIN, accountTable, accountTable.idCol, projectTable, projectTable.accountIdCol);
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a LEFT JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void rightJoin() {
        Join joinQuery = new Join(Type.RIGHT_JOIN, accountTable, accountTable.idCol, projectTable, projectTable.accountIdCol);
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a RIGHT JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void fullJoin() {
        Join joinQuery = new Join(Type.FULL_JOIN, accountTable, accountTable.idCol, projectTable, projectTable.accountIdCol);
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a FULL JOIN project p ON a.id = p.account_id", resolved.toString());
    }

    @Test
    void crossJoin() {
        Join<Account, Project> joinQuery = new Join<>(Type.CROSS_JOIN, accountTable, accountTable.idCol, projectTable, projectTable.accountIdCol);
        Expr<SqlJoinRows<Account, Project>> resolved = joinQuery.resolve(emptyMap());
        assertEquals("account a CROSS JOIN project p", resolved.toString());
    }
}
