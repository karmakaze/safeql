package org.keithkim.demo.quicklog;

import org.jdbi.v3.core.mapper.JoinRow;
import org.keithkim.demo.Database;
import org.keithkim.safeql.Join;
import org.keithkim.safeql.Registry;
import org.keithkim.safeql.sql.SqlTable;

import java.util.List;

import static java.util.Arrays.asList;

public class QuicklogMain {
    public static void main(String[] args) {
        String jdbcUrl = System.getenv("JDBC_URL");
        String dbUser = System.getenv("DB_USER");
        String dbPassword = System.getenv("DB_PASSWORD");
        Database db = new Database(jdbcUrl, dbUser, dbPassword);
        Registry.register(Account.class, db);
        Registry.register(Project.class, db);

        SqlTable accountTable = new SqlTable<>(Account.class, "a");
        SqlTable projectTable = new SqlTable<>(Project.class, "p");

        Join<Account, Project> accountJoinProject = new Join(accountTable, projectTable).where(new Join.Cond2());

        List<JoinRow> accounts = Registry.using(asList(accountTable, projectTable), handle -> {
            return handle.createQuery("SELECT a.id a_id, a.full_name a_full_name, a.email a_email, a.plan_name a_plan_name, a.expires a_expires, "+
                    "p.id p_id, p.account_id p_account_id, p.name p_name, p.domain p_domain "+
                    "FROM account a JOIN project p ON a.id = p.account_id "+
                    "WHERE p.account_id IN (<account_ids>)")
                    .bindList("account_ids", asList(5483L, 9999L, 412885L, 412895L, 412896L, 412897L, 412898L))
                    .mapTo(JoinRow.class)
                    .list();
        });

        for (JoinRow row : accounts) {
            Account account = row.get(Account.class);
            Project project = row.get(Project.class);
            System.out.println(account +" :has: "+ project);
        }
    }
}
