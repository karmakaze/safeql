package org.keithkim.safeql.test;

import org.keithkim.safeql.schema.Entity;

import java.beans.ConstructorProperties;
import java.time.Instant;

public class Project0 extends Entity<Long> {
  public final ProjectRow row;

  @ConstructorProperties({"id", "accountId", "name", "domain", "createdAt", "updatedAt"})
  public Project0(Long id, Long accountId, String name, String domain, Instant createdAt,
                 Instant updatedAt) {
    row = new ProjectRow(id, accountId, name, domain, createdAt, updatedAt);
  }

  public Long id() {
    return row.id;
  }

  public String toString() {
    StringBuilder buffer = new StringBuilder("Project{");
    row.intoString(buffer);
    buffer.append("}");
    return buffer.toString();
  }

  public static class Table extends org.keithkim.safeql.schema.Table<Project0> {
    public final Id idCol = new Id();

    public final AccountId accountIdCol = new AccountId();

    public final Name nameCol = new Name();

    public final Domain domainCol = new Domain();

    public final CreatedAt createdAtCol = new CreatedAt();

    public final UpdatedAt updatedAtCol = new UpdatedAt();

    public Table(String tableExpr, String alias) {
      super(Project0.class, tableExpr, alias);
    }

    public Id idCol(String alias) {
      return new Id(alias);
    }

    public AccountId accountIdCol(String alias) {
      return new AccountId(alias);
    }

    public Name nameCol(String alias) {
      return new Name(alias);
    }

    public Domain domainCol(String alias) {
      return new Domain(alias);
    }

    public CreatedAt createdAtCol(String alias) {
      return new CreatedAt(alias);
    }

    public UpdatedAt updatedAtCol(String alias) {
      return new UpdatedAt(alias);
    }

    public class Id extends SqlColumn<Long> {
      public Id() {
        super("id");
      }

      public Id(String alias) {
        super(alias);
      }
    }

    public class AccountId extends SqlColumn<Long> {
      public AccountId() {
        super("account_id");
      }

      public AccountId(String alias) {
        super("account_id", alias);
      }
    }

    public class Name extends SqlColumn<String> {
      public Name() {
        super("name");
      }

      public Name(String alias) {
        super(alias);
      }
    }

    public class Domain extends SqlColumn<String> {
      public Domain() {
        super("domain");
      }

      public Domain(String alias) {
        super(alias);
      }
    }

    public class CreatedAt extends SqlColumn<Instant> {
      public CreatedAt() {
        super("created_at");
      }

      public CreatedAt(String alias) {
        super(alias);
      }
    }

    public class UpdatedAt extends SqlColumn<Instant> {
      public UpdatedAt() {
        super("updated_at");
      }

      public UpdatedAt(String alias) {
        super(alias);
      }
    }
  }
}
