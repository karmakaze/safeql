package org.keithkim.safeql.sample.projects;

import org.keithkim.safeql.statement.TableDbRegistry;

import java.util.*;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toSet;

public class Accounts0 extends ArrayList<Account> {
    public Accounts0(List<Account> accounts) {
        super(accounts);
    }

    public Set<Long> ids() {
        return this.stream().map(account -> account.row.id).collect(toSet());
    }

    public Map<Long, Account> byId() {
        Map<Long, Account> accountById = new HashMap<>(size());
        for (Account account : this) {
            accountById.put(account.row.id, account);
        }
        return accountById;
    }

    public Accounts0 andWhere(String cond) {
        // TODO more filtering
        return this;
    }

    public Projects0 loadProjects() {
        Map<Long, Account> accountById = byId();
        Set<Long> accountIds = accountById.keySet();

        Projects0 projects = new Projects0();
        projects = projects.whereAccountIdIn(accountIds);

        for (Project project : projects) {
            System.out.println("project.row.accountId=" + project.row.accountId);
            System.out.println("project.row.accountId.class.name=" + project.row.accountId.getClass().getCanonicalName());
            Account account = accountById.get(project.row.accountId);
            if (account != null) {
                account.addProject(project);
            }
        }

        return projects;
    }

    public static Accounts0 where(String cond) {
        List<Account> accounts = TableDbRegistry.using(singletonList(new Account.Table("account", null)), handle -> {
            String whereClause = "";
            if (cond != null && !cond.isEmpty()) {
                whereClause = " WHERE " + cond;
            }
            return handle.createQuery("SELECT * FROM account"+ whereClause)
                    .mapTo(Account.class)
                    .list();
        });
        return new Accounts0(accounts);
    }
}
