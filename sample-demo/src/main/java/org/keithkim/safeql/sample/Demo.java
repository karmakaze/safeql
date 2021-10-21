package org.keithkim.safeql.sample;

import org.jdbi.v3.core.mapper.JoinRow;
import org.jdbi.v3.core.mapper.JoinRowMapper;
import org.keithkim.safeql.sample.projects.Account;
import org.keithkim.safeql.sample.projects.Accounts0;
import org.keithkim.safeql.sample.projects.Project;
import org.keithkim.safeql.sample.projects.Projects0;
import org.keithkim.moja.monad.Async;
import org.keithkim.moja.monad.Multi;
import org.keithkim.safeql.query.Join;
import org.keithkim.safeql.statement.Database;
import org.keithkim.safeql.statement.TableDbRegistry;
import org.keithkim.safeql.statement.RawQueryStatement;

import java.util.List;

import static java.util.Arrays.asList;

public class Demo {
    public void demoCompose() {
        RawQueryStatement<Account> select = new RawQueryStatement<>("SELECT * FROM account", Account.class);
        Async<Multi<Account>> asyncAccounts = select.multiAsync();

        Async<Accounts0> asyncAccountsAndProjects = asyncAccounts.then(multiAccount -> {
            return Async.async(() -> {
                Accounts0 as = new Accounts0(multiAccount.toList());
                as.loadProjects();
                return as;
            });
        });
        Accounts0 accountsAndProjects = asyncAccountsAndProjects.join();

        for (Account account : accountsAndProjects) {
            System.out.println("" + account);
        }
    }

    public void demoJoin() {
        Account.Table accountTable = new Account.Table("account", "a");
        Project.Table projectTable = new Project.Table("project", "p");

        Join<Account, Project> accountJoinProject = new Join(accountTable, projectTable).where(new Join.Cond2());

        List<JoinRow> accountAndProjects = TableDbRegistry.using(asList(accountTable, projectTable), handle -> {
            handle.registerRowMapper(JoinRowMapper.forTypes(Account.class, Project.class));

            return handle.createQuery("SELECT a.id a_id, a.full_name a_full_name, a.email a_email, a.plan_name a_plan_name, a.expires_at a_expires_at, "+
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

    public Accounts0 demoAccountsWhere() {
        Account.Table accountTable = new Account.Table("account", null);
        Accounts0 accounts = new Accounts0(accountTable.where("id >= 1000"));
        return accounts;
    }

    public Projects0 demoAccountsLoadProjects(Accounts0 accounts) {
        Projects0 projects = accounts.loadProjects();
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
        String jdbcUrl = System.getenv("JDBC_URL");
        String dbUser = System.getenv("DB_USER");
        String dbPassword = System.getenv("DB_PASSWORD");
        Database db = new Database(jdbcUrl, dbUser, dbPassword);
        TableDbRegistry.registerDefault(db);

        Demo demo = new Demo();
        demo.demoCompose();
//        demoMain.demoJoin();

//        Accounts accounts = demoMain.demoAccountsWhere();
//        Projects projects = demoMain.demoAccountsLoadProjects(accounts);
//        for (Project project : projects) {
//            System.out.println("" + project);
//        }
//        for (Account account : accounts) {
//            System.out.println("" + account);
//        }
    }
}
