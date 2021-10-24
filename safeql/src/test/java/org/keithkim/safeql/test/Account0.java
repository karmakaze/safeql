package org.keithkim.safeql.test;

import java.beans.ConstructorProperties;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import org.keithkim.safeql.schema.Entity;

public class Account0 extends Entity<Long> {
    public final AccountRow row;

    public final Map<Long, Project0> projects = new HashMap<>();

    @ConstructorProperties({"id", "fullName", "email", "planName", "expiresAt", "createdAt", "updatedAt"})
    public Account0(Long id, String fullName, String email, String planName, Instant expiresAt,
                    Instant createdAt, Instant updatedAt) {
        row = new AccountRow(id, fullName, email, planName, expiresAt, createdAt, updatedAt);
    }

    public Long id() {
        return row.id;
    }

    public LinkedHashMap<Long, Project0> projects() {
        LinkedHashMap<Long, Project0> projects = (LinkedHashMap<Long, Project0>) childrenByClass.get(Project0.class);
        if (projects == null) {
            projects = new LinkedHashMap<>();
            childrenByClass.put(Project0.class, projects);
        };
        return projects;
    }

    public void addProject0(Project0 project) {
        projects().put(project.row.id, project);
    }

    public String toString() {
        StringBuilder buffer = new StringBuilder("Account{");
        row.intoString(buffer);
        String projectsString = projects().values().toString();
        buffer.append(", projects:[ " + projectsString.substring(1, projectsString.length() - 1) + " ]");
        buffer.append("}");
        return buffer.toString();
    }

    public static class Table extends org.keithkim.safeql.schema.Table<Account0> {
        public final Id idCol = new Id();

        public final FullName fullNameCol = new FullName();

        public final Email emailCol = new Email();

        public final PlanName planNameCol = new PlanName();

        public final ExpiresAt expiresAtCol = new ExpiresAt();

        public final CreatedAt createdAtCol = new CreatedAt();

        public final UpdatedAt updatedAtCol = new UpdatedAt();

        public Table(String tableExpr, String alias) {
            super(Account0.class, tableExpr, alias);
        }

        public Id idCol(String alias) {
            return new Id(alias);
        }

        public FullName fullNameCol(String alias) {
            return new FullName(alias);
        }

        public Email emailCol(String alias) {
            return new Email(alias);
        }

        public PlanName planNameCol(String alias) {
            return new PlanName(alias);
        }

        public ExpiresAt expiresAtCol(String alias) {
            return new ExpiresAt(alias);
        }

        public CreatedAt createdAtCol(String alias) {
            return new CreatedAt(alias);
        }

        public UpdatedAt updatedAtCol(String alias) {
            return new UpdatedAt(alias);
        }

        public class Id extends Table.SqlColumn<Long> {
            public Id() {
                super("id");
            }

            public Id(String alias) {
                super(alias);
            }
        }

        public class FullName extends Table.SqlColumn<String> {
            public FullName() {
                super("full_name");
            }

            public FullName(String alias) {
                super(alias);
            }
        }

        public class Email extends Table.SqlColumn<String> {
            public Email() {
                super("email");
            }

            public Email(String alias) {
                super(alias);
            }
        }

        public class PlanName extends Table.SqlColumn<String> {
            public PlanName() {
                super("plan_name");
            }

            public PlanName(String alias) {
                super(alias);
            }
        }

        public class ExpiresAt extends Table.SqlColumn<Instant> {
            public ExpiresAt() {
                super("expires_at");
            }

            public ExpiresAt(String alias) {
                super(alias);
            }
        }

        public class CreatedAt extends Table.SqlColumn<Instant> {
            public CreatedAt() {
                super("created_at");
            }

            public CreatedAt(String alias) {
                super(alias);
            }
        }

        public class UpdatedAt extends Table.SqlColumn<Instant> {
            public UpdatedAt() {
                super("updated_at");
            }

            public UpdatedAt(String alias) {
                super(alias);
            }
        }
    }
}
