package org.keithkim.safeql.test;

import java.time.Instant;

public class AccountRow {
  public Long id;

  public String fullName;

  public String email;

  public String planName;

  public Instant expiresAt;

  public Instant createdAt;

  public Instant updatedAt;

  public AccountRow(Long id, String fullName, String email, String planName, Instant expiresAt,
      Instant createdAt, Instant updatedAt) {
    this.id = id;
    this.fullName = fullName;
    this.email = email;
    this.planName = planName;
    this.expiresAt = expiresAt;
    this.createdAt = createdAt;
    this.updatedAt = updatedAt;
  }

  public String toString() {
    return "AccountRow{" + intoString(new StringBuilder()) + "}";
  }

  public StringBuilder intoString(StringBuilder buffer) {
    buffer.append(String.format("id:%s, fullName:%s, email:%s, planName:%s, expiresAt:%s, createdAt:%s, updatedAt:%s",
        id, fullName, email, planName, expiresAt, createdAt, updatedAt));
    return buffer;
  }
}
