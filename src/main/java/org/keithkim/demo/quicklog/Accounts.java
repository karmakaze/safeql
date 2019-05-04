package org.keithkim.demo.quicklog;

import org.keithkim.safeql.Registry;

import java.util.*;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toSet;

public class Accounts extends ArrayList<Account> {
    public Accounts(List<Account> accounts) {
        super(accounts);
    }

    public Set<Long> ids() {
        return this.stream().map(account -> account.id).collect(toSet());
    }

    public Map<Long, Account> byId() {
        Map<Long, Account> accountById = new HashMap<>(size());
        for (Account account : this) {
            accountById.put(account.id, account);
        }
        return accountById;
    }

    public Accounts andWhere(String cond) {
        // TODO more filtering
        return this;
    }

    public Projects loadProjects() {
        Map<Long, Account> accountById = byId();
        Set<Long> accountIds = accountById.keySet();

        Projects projects = new Projects();
        projects = projects.whereAccountIdIn(accountIds);

        for (Project project : projects) {
            Account account = accountById.get(project.accountId);
            if (account != null) {
                account.addProject(project);
            }
        }

        return projects;
    }

    public static Accounts where(String cond) {
        List<Account> accounts = Registry.using(singletonList(new Account.Table("account", null)),handle -> {
            String whereClause = "";
            if (cond != null && !cond.isEmpty()) {
                whereClause = " WHERE " + cond;
            }
            return handle.createQuery("SELECT * FROM account"+ whereClause)
                    .mapTo(Account.class)
                    .list();
        });
        return new Accounts(accounts);
    }
}
