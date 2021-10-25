package org.keithkim.safeql.sample.projects;

import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.predicate.Predicate;
import org.keithkim.safeql.predicate.Predicates;
import org.keithkim.safeql.schema.Entities;
import org.keithkim.safeql.schema.Table;

import java.util.*;

import static java.util.stream.Collectors.toSet;

public class Projects0 extends Entities<Projects0, Project> {
    @Override
    public Projects0 newEntities(List<Project> projects) {
        return new Projects0(table, projects);
    }

    public Projects0() {
        super(Project.Table.DEFAULT);
    }

    public Projects0(Table table) {
        super(table);
    }

    public Projects0(List<Project> projects) {
        super(Project.Table.DEFAULT, projects);
    }

    public Projects0(Table table, List<Project> projects) {
        super(table, projects);
    }

    public Set<Long> ids() {
        return this.stream().map(project -> project.row.id).collect(toSet());
    }

    public Projects0 whereAccountIdIn(Set<Long> accountIds) {
        SortedMap<String, Object> bindVars = new TreeMap<String, Object>() {{ put("account_ids", accountIds); }};
//        Predicate predicate = new Predicate("account_id IN (<account_ids>)", bindVars) {};
        Expr<String> right = Expr.expr(":account_ids", bindVars);
        Predicate predicate = Predicates.IN(new Expr("account_id"), right);
        return where(predicate);
//        List<Project> projects = TableDbRegistry.using(singletonList(new Account.Table("account", null)), handle -> {
//            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
//            return handle.createQuery("SELECT * FROM project WHERE account_id IN (<account_ids>)")
//                    .bindList("account_ids", new ArrayList<>(accountIds))
//                    .mapTo(Project.class)
//                    .list();
//        });
//        return newEntities(projects);
    }

    public Projects0 where(String cond) {
        return where(new Predicate(cond) {});
//        List<Project> projects = TableDbRegistry.using(singletonList(new Project.Table("project", null)), handle -> {
//            handle.registerRowMapper(ConstructorMapper.factory(Project.class));
//            String whereClause = "";
//            if (cond != null && !cond.isEmpty()) {
//                whereClause = " WHERE " + cond;
//            }
//            return handle.createQuery("SELECT * FROM project"+ whereClause)
//                    .mapTo(Project.class)
//                    .list();
//        });
//        return new Projects0(table, projects);
    }
}
