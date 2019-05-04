package org.keithkim.demo.quicklog;

import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

import java.beans.ConstructorProperties;

public class Project extends SqlEntity<Long> {
    public static class Table extends SqlTable<Project> {
        public Table(String tableExpr, String alias) {
            super(Project.class, tableExpr, alias);
        }

        public class Id extends SqlColumn<Long> {
            public Id(String alias) {
                super("id", alias);
            }
        }
        public class AccountId extends SqlColumn<Long> {
            public AccountId(String alias) {
                super("account_id", alias);
            }
        }
        public class Name extends SqlColumn<String> {
            public Name(String alias) {
                super("name", alias);
            }
        }
        public class Domain extends SqlColumn<String> {
            public Domain(String alias) {
                super("domain", alias);
            }
        }

        public final Id idCol = new Id(null);
        public final AccountId accountIdCol = new AccountId(null);
        public final Name nameCol = new Name(null);
        public final Domain domainCol = new Domain(null);

        public Id idCol(String alias) {
            return new Id(alias);
        }
        public AccountId accountIdCol(String alias) {
            return new AccountId(alias);
        }
        public Name nameCol(String alias) {
            return new Name(alias);
        }
        public Domain domainCol(String alias) {
            return new Domain(alias);
        }
    }

    public long id;
    public long accountId;
    public String name;
    public String domain;

    @ConstructorProperties({"id", "account_id", "name", "domain"})
    public Project(long id, long accountId, String name, String domain) {
        this.id = id;
        this.accountId = accountId;
        this.name = name;
        this.domain = domain;
    }

    public String toString() {
        return String.format("Project<id:%d, accountId:%d, name:%s, domain:%s>", id, accountId, name, domain);
    }
}
