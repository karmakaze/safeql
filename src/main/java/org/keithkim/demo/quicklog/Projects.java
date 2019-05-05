package org.keithkim.demo.quicklog;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.safeql.sql.expression.Registry;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toSet;

public class Projects extends ArrayList<Project> {
    public Projects() {
        super();
    }

    public Projects(List<Project> projects) {
        super(projects);
    }

    public Set<Long> ids() {
        return this.stream().map(project -> project.id).collect(toSet());
    }

    public Projects whereAccountIdIn(Set<Long> accountIds) {
        List<Project> projects = Registry.using(singletonList(new Account.Table("account", null)), handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
            return handle.createQuery("SELECT * FROM project WHERE account_id IN (<account_ids>)")
                    .bindList("account_ids", new ArrayList<>(accountIds))
                    .mapTo(Project.class)
                    .list();
        });
        return new Projects(projects);
    }

    public Projects where(String cond) {
        List<Project> projects = Registry.using(singletonList(new Project.Table("project", null)), handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
            String whereClause = "";
            if (cond != null && !cond.isEmpty()) {
                whereClause = " WHERE " + cond;
            }
            return handle.createQuery("SELECT * FROM project"+ whereClause)
                    .mapTo(Project.class)
                    .list();
        });
        return new Projects(projects);
    }
}
