package org.keithkim.safeql.sample.projects;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.safeql.statement.TableDbRegistry;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toSet;

public class Projects00 extends ArrayList<Project> {
    public Projects00() {
        super();
    }

    public Projects00(List<Project> projects) {
        super(projects);
    }

    public Set<Long> ids() {
        return this.stream().map(project -> project.row.id).collect(toSet());
    }

    public Projects00 whereAccountIdIn(Set<Long> accountIds) {
        List<Project> projects = TableDbRegistry.using(singletonList(new Account.Table("account", null)), handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
            return handle.createQuery("SELECT * FROM project WHERE account_id IN (<account_ids>)")
                    .bindList("account_ids", new ArrayList<>(accountIds))
                    .mapTo(Project.class)
                    .list();
        });
        return new Projects00(projects);
    }

    public Projects00 where(String cond) {
        List<Project> projects = TableDbRegistry.using(singletonList(new Project.Table("project", null)), handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
            String whereClause = "";
            if (cond != null && !cond.isEmpty()) {
                whereClause = " WHERE " + cond;
            }
            return handle.createQuery("SELECT * FROM project"+ whereClause)
                    .mapTo(Project.class)
                    .list();
        });
        return new Projects00(projects);
    }
}
