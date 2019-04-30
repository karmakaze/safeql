package org.keithkim.demo.quicklog;

import org.keithkim.typeql.Col;
import org.keithkim.typeql.Entity;
import org.keithkim.typeql.Table;

import java.beans.ConstructorProperties;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

public class Account extends Entity<Long> {
    public final Col<Account, Long> idCol;
    public final Col<Account, String> fullNameCol;

    public Long id;
    public String fullName;
    public String email;
    public String planName;
    public Instant expires;

    public Map<Long, Project> projects = new HashMap<>();

    @ConstructorProperties({"id", "full_name", "email", "plan_name", "expires"})
    public Account(long id, String fullName, String email, String planName, Instant expires) {
        super("account");
        this.id = id;
        this.fullName = fullName;
        this.email = email;
        this.planName = planName;
        this.expires = expires;

        Table<Account> table = new Table<>(Account.class);
        idCol = new Col<>(Account.class, table.alias, "id");
        fullNameCol = new Col<>(Account.class, table.alias, "full_name");
    }

    void addProject(Project project) {
        this.projects.put(project.id, project);
    }

    public String toString() {
        return String.format("Account<id:%d, fullName:%s, email:%s, planName:%s, expires:%s, projects: %s>",
                id, fullName, email, planName, expires, projects.values());
    }
}
