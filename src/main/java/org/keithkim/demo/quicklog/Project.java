package org.keithkim.demo.quicklog;

import org.keithkim.safeql.sql.SqlColumn;
import org.keithkim.safeql.sql.SqlEntity;
import org.keithkim.safeql.sql.SqlTable;

import java.beans.ConstructorProperties;

public class Project extends SqlEntity<Long> {
    public final SqlColumn<Project, Long> idCol;
    public final SqlColumn<Project, String> nameCol;

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

        SqlTable<Project> table = new SqlTable<>(Project.class);
        idCol = new SqlColumn<>(this, "id");
        nameCol = new SqlColumn<>(this, "name");
    }

    public String toString() {
        return String.format("Project<id:%d, accountId:%d, name:%s, domain:%s>", id, accountId, name, domain);
    }
}
