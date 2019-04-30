package org.keithkim.demo.quicklog;

import org.keithkim.typeql.Col;
import org.keithkim.typeql.Entity;
import org.keithkim.typeql.Table;

import java.beans.ConstructorProperties;

public class Project extends Entity<Long> {
    public final Col<Project, Long> idCol;
    public final Col<Project, String> nameCol;

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

        Table<Project> table = new Table<>(Project.class);
        idCol = new Col<>(Project.class, table.alias, "id");
        nameCol = new Col<>(Project.class, table.alias, "name");
    }

    public String toString() {
        return String.format("Project<id:%d, accountId:%d, name:%s, domain:%s>", id, accountId, name, domain);
    }
}
