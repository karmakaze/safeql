package org.keithkim.demo.quicklog;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.demo.Database;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

public class Projects extends ArrayList<Project> {
    private final Database db;

    public Projects(Database db) {
        super();
        this.db = db;
    }

    public Projects(Database db, List<Project> projects) {
        super(projects);
        this.db = db;
    }

    public Set<Long> ids() {
        return this.stream().map(project -> project.id).collect(toSet());
    }

    public Projects whereAccountIdIn(Set<Long> accountIds) {
        List<Project> projects = db.jdbi.withHandle(handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
            return handle.createQuery("SELECT * FROM project WHERE account_id IN (<account_ids>)")
                    .bindList("account_ids", new ArrayList<>(accountIds))
                    .mapTo(Project.class)
                    .list();
        });
        return new Projects(db, projects);
    }

    public Projects where(String cond) {
        List<Project> projects = db.jdbi.withHandle(handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
            String whereClause = "";
            if (cond != null && !cond.isEmpty()) {
                whereClause = " WHERE " + cond;
            }
            return handle.createQuery("SELECT * FROM project"+ whereClause)
                    .mapTo(Project.class)
                    .list();
        });
        return new Projects(db, projects);
    }
}
