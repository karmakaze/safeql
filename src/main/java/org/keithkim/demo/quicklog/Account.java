package org.keithkim.demo.quicklog;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;

import java.beans.ConstructorProperties;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

public class Account extends SqlEntity<Long> {
    public class Id extends SqlColumn<Account, Long>{
        public Id() {
            super(Account.this, "id");
        }
    }
    public class FullName extends SqlColumn<Account, String>{
        public FullName() {
            super(Account.this, "full_name");
        }
    }

    public final Id idCol = new Id();
    public final FullName fullNameCol = new FullName();

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
    }

    void addProject(Project project) {
        this.projects.put(project.id, project);
    }

    public String toString() {
        return String.format("Account<id:%d, fullName:%s, email:%s, planName:%s, expires:%s, projects: %s>",
                id, fullName, email, planName, expires, projects.values());
    }
}
