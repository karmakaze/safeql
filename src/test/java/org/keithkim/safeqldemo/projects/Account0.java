package org.keithkim.safeqldemo.projects;

import org.keithkim.safeql.schema.Entity;

import java.beans.ConstructorProperties;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;

public class Account0 extends Entity<Long> {
    public final AccountRow0 row;

    @Override
    public Long id() {
        return row.id;
    }

    public Map<Long, Project> projects() {
        LinkedHashMap<Long, Project> projects = (LinkedHashMap<Long, Project>) childrenByClass.get(Project.class);
        if (projects == null) {
            projects = new LinkedHashMap<>();
            childrenByClass.put(Project.class, projects);
        }
        return projects;
    }

    public void addProject(Project project) {
        projects().put(project.row.id, project);
    }

    @ConstructorProperties({"id", "full_name", "email", "plan_name", "expires_at"})
    public Account0(long id, String fullName, String email, String planName, Instant expiresAt) {
        row = new AccountRow0(id, fullName, email, planName, expiresAt);
    }


    @Override
    public String toString() {
        StringBuilder buffer = new StringBuilder("Account<");
        row.intoString(buffer);
        String projects = String.format("%s", projects().values());
        buffer.append(", projects:[ " + projects.substring(1, projects.length() - 1) + " ]");
        buffer.append(">");
        return buffer.toString();
    }

    public static class Table extends org.keithkim.safeql.schema.Table<Account> {
        public Table(String tableExpr, String alias) {
            super(Account.class, tableExpr, alias);
        }

        public class Id extends SqlColumn<Long> {
            public Id() {
                super("id");
            }
        }
        public class FullName extends SqlColumn<String> {
            public FullName() {
                super("full_name");
            }
        }

        public final Id idCol = new Id();
        public final FullName fullNameCol = new FullName();

        public Accounts0 all() {
            return where(null);
        }

        public Accounts0 where(String criteria) {
            return new Accounts0(super.where(criteria));
        }
    }
}
