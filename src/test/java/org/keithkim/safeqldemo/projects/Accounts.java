package org.keithkim.safeqldemo.projects;

import org.keithkim.safeql.schema.Entities;
import org.keithkim.safeql.schema.Table;

import java.util.*;

import static java.util.stream.Collectors.toSet;

public class Accounts extends Entities<Accounts, Account> {
    public Accounts(String alias) {
        super(new Account.Table("account", alias));
    }

    public Accounts(Table<Account> table, List<Account> accounts) {
        super(table, accounts);
    }

    @Override
    public Accounts newEntities(List<Account> entities) {
        return new Accounts(table, entities);
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

    public Accounts andWhere(String cond) {
        // TODO more filtering
        return this;
    }

    public Projects0 loadProjects() {
        Map<Long, Account> accountById = byId();
        Set<Long> accountIds = accountById.keySet();

        Projects0 projects = new Projects0();
        projects = projects.whereAccountIdIn(accountIds);

        for (Project project : projects) {
            Account account = accountById.get(project.row.accountId);
            if (account != null) {
                account.addProject(project);
            }
        }

        return projects;
    }

//    public <EE extends Projects0, E extends Project> EE loadHasMany(EE entities) {
//        Map<Long, Account> accountById = byId();
//        Set<Long> accountIds = accountById.keySet();
//
//        entities.whereAccountIdIn(accountIds);
//
//        entities.forEach(entity -> {
//            E parent = accountById.get(entity.row.accountId);
//            if (parent != null) {
//                parent.addChild(entity);
//            }
//        });
//
//        return entities;
//    }

    public static Accounts where(String cond) {
        Accounts accounts = new Accounts(new Account.Table("account", null), new ArrayList<>());
        return accounts.where(cond);
    }
}
