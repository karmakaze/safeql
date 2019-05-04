package org.keithkim.demo.quicklog;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

import java.beans.ConstructorProperties;

public class Project extends SqlEntity<Long> {
    public class Id extends SqlColumn<Project, Long>{
        public Id() {
            super(Project.this, "id");
        }
    }
    public class Name extends SqlColumn<Project, String>{
        public Name() {
            super(Project.this, "name");
        }
    }

    public final Id idCol = new Id();
    public final Name nameCol = new Name();

    public long id;
    public long accountId;
    public String name;
    public String domain;

    @ConstructorProperties({"id", "account_id", "name", "domain"})
    public Project(long id, long accountId, String name, String domain) {
        super("project");
        this.id = id;
        this.accountId = accountId;
        this.name = name;
        this.domain = domain;
    }

    public String toString() {
        return String.format("Project<id:%d, accountId:%d, name:%s, domain:%s>", id, accountId, name, domain);
    }
}
