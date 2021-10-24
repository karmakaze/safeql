package org.keithkim.safeql.test;

import java.time.Instant;

public class ProjectRow {
  public Long id;

  public Long accountId;

  public String name;

  public String domain;

  public Instant createdAt;

  public Instant updatedAt;

  public ProjectRow(Long id, Long accountId, String name, String domain, Instant createdAt,
      Instant updatedAt) {
    this.id = id;
    this.accountId = accountId;
    this.name = name;
    this.domain = domain;
    this.createdAt = createdAt;
    this.updatedAt = updatedAt;
  }

  public String toString() {
    return "ProjectRow{" + intoString(new StringBuilder()) + "}";
  }

  public StringBuilder intoString(StringBuilder buffer) {
    buffer.append(String.format("id:%s, accountId:%s, name:%s, domain:%s, createdAt:%s, updatedAt:%s",
        id, accountId, name, domain, createdAt, updatedAt));
    return buffer;
  }
}
