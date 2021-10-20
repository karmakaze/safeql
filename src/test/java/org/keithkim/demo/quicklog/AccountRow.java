package org.keithkim.demo.quicklog;

import java.time.Instant;

public class AccountRow {
    public Long id;
    public String fullName;
    public String email;
    public String planName;
    public Instant expiresAt;

    public AccountRow(long id, String fullName, String email, String planName, Instant expiresAt) {
        this.id = id;
        this.fullName = fullName;
        this.email = email;
        this.planName = planName;
        this.expiresAt = expiresAt;
    }

    @Override
    public String toString() {
        return "AccountRow<" + intoString(new StringBuilder()) + ">";
    }

    public StringBuilder intoString(StringBuilder buffer) {
        buffer.append(String.format("id:%d, fullName:%s, email:%s, planName:%s, expiresAt:%s",
                id, fullName, email, planName, expiresAt));
        return buffer;
    }
}
