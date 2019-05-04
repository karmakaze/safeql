package org.keithkim.demo;

import org.jdbi.v3.core.mapper.JoinRow;
import org.jdbi.v3.core.mapper.JoinRowMapper;
import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.demo.photos.Photo;
import org.keithkim.demo.photos.User;
import org.keithkim.demo.photos.UserJoinPhoto;
import org.keithkim.demo.quicklog.Account;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.sql.SqlTable;

import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;

public class Main {
    public static void main(String[] args) {
        String jdbcUrl = System.getenv("JDBC_URL");
        String dbUser = System.getenv("DB_USER");
        String dbPassword = System.getenv("DB_PASSWORD");
        Database db = new Database(jdbcUrl, dbUser, dbPassword);
        List<JoinRow> accounts = db.jdbi.withHandle(handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Account.class, "a_"));
            handle.registerRowMapper(ConstructorMapper.factory(Project.class, "p_"));
            handle.registerRowMapper(JoinRowMapper.forTypes(Account.class, Project.class));
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
        UserJoinPhoto up = new UserJoinPhoto(new SqlTable<>(User.class, "u"), new SqlTable<>(Photo.class, "p"));
        System.out.println(up.resolve(emptyMap()));
    }
}
