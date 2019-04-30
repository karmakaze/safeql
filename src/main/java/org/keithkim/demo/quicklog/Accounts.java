package org.keithkim.demo.quicklog;

import org.jdbi.v3.core.mapper.reflect.ConstructorMapper;
import org.keithkim.demo.Database;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

public class Accounts extends ArrayList<Account> {
    private final Database db;

    public Accounts(Database db) {
        super();
        this.db = db;
    }

    public Accounts(Database db, List<Account> accounts) {
        super(accounts);
        this.db = db;
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

    public Projects loadProjects() {
        Map<Long, Account> accountById = byId();
        Set<Long> accountIds = accountById.keySet();

        Projects projects = new Projects(db);
        projects = projects.whereAccountIdIn(accountIds);

        for (Project project : projects) {
            Account account = accountById.get(project.accountId);
            if (account != null) {
                account.addProject(project);
            }
        }

        return projects;
    }

    public Accounts where(String cond) {
        List<Account> accounts = db.jdbi.withHandle(handle -> {
            handle.registerRowMapper(ConstructorMapper.factory(Account.class));
            String whereClause = "";
            if (cond != null && !cond.isEmpty()) {
                whereClause = " WHERE " + cond;
            }
            return handle.createQuery("SELECT * FROM account"+ whereClause)
                    .mapTo(Account.class)
                    .list();
        });
        return new Accounts(db, accounts);
    }
}
