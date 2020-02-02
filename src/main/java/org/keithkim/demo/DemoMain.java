package org.keithkim.demo;

import org.jdbi.v3.core.mapper.JoinRow;
import org.jdbi.v3.core.mapper.JoinRowMapper;
import org.keithkim.demo.quicklog.*;
import org.keithkim.safeql.query.Join;
import org.keithkim.safeql.statement.Database;
import org.keithkim.safeql.statement.Registry;
import org.keithkim.safeql.statement.RawQueryStatement;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.Arrays.asList;

public class DemoMain {
    public void demoCompose() {
        RawQueryStatement<Account> select = new RawQueryStatement<>("SELECT * FROM account", Account.class);
        CompletableFuture<List<Account>> asyncAccounts = select.listAsync();

        for (Account account : asyncAccounts.join()) {
            System.out.println("" + account);
        }
    }

    public void demoJoin() {
        Account.Table accountTable = new Account.Table("account", "a");
        Project.Table projectTable = new Project.Table("project", "p");

        Join<Account, Project> accountJoinProject = new Join(accountTable, projectTable).where(new Join.Cond2());

        List<JoinRow> accountAndProjects = Registry.using(asList(accountTable, projectTable), handle -> {
            handle.registerRowMapper(JoinRowMapper.forTypes(Account.class, Project.class));

            return handle.createQuery("SELECT a.id a_id, a.full_name a_full_name, a.email a_email, a.plan_name a_plan_name, a.expires a_expires, "+
                    "p.id p_id, p.account_id p_account_id, p.name p_name, p.domain p_domain "+
                    "FROM account a JOIN project p ON a.id = p.account_id "+
                    "WHERE p.account_id IN (<account_ids>)")
                    .bindList("account_ids", asList(5483L, 9999L, 412885L, 412895L, 412896L, 412897L, 412898L))
                    .mapTo(JoinRow.class)
                    .list();
        });

        for (JoinRow row : accountAndProjects) {
            Account account = row.get(Account.class);
            Project project = row.get(Project.class);
            System.out.println(account +" :has: "+ project);
        }
    }

    public Accounts demoAccountsWhere() {
        Account.Table accountTable = new Account.Table("account", null);
        Accounts accounts = accountTable.where("id >= 1000");
        return accounts;
    }

    public Projects demoAccountsLoadProjects(Accounts accounts) {
        Projects projects = accounts.loadProjects();
        return projects;
    }

    public static void sleep(long millis) {
        try {
            Thread.sleep(250L);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) {
        // for setting db
        DataSourceEnvironment.setup();
//        String jdbcUrl = System.getenv("JDBC_URL");
//        String dbUser = System.getenv("DB_USER");
//        String dbPassword = System.getenv("DB_PASSWORD");
//        Database db = new Database(jdbcUrl, dbUser, dbPassword);
//        Registry.registerDefault(db);

        DemoMain demoMain = new DemoMain();
        demoMain.demoCompose();

//        CompletableFuture<Accounts> asyncAccounts = CompletableFuture.supplyAsync(demoMain::demoAccountsWhere);
//        asyncAccounts.thenAcceptAsync(accounts -> {
//            for (Account account : accounts) {
//                System.out.println(account);
//                sleep(100L);
//            }
//        });
//        CompletionStage<Projects> asyncProjects = Async.pipeline(asyncAccounts, (accounts) -> demoMain.demoAccountsLoadProjects(accounts));
//        asyncProjects.thenAccept(projects -> {
//            for (Project project : projects) {
//                System.out.println(project);
//                sleep(100L);
//            }
//        });
//        asyncAccounts.join();
//        asyncProjects.toCompletableFuture().join();
//
//        demoMain.demoJoin();
    }
}
