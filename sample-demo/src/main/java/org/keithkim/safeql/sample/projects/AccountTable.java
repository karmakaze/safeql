package org.keithkim.safeql.sample.projects;

import org.keithkim.safeql.annotation.Column;
import org.keithkim.safeql.annotation.Table;

import java.time.Instant;

import static org.keithkim.safeql.annotation.ColumnOption.PRIMARY_KEY;

@Table
public abstract class AccountTable {
    @Column(name = "id", options = {PRIMARY_KEY})
    Long id;

    @Column
    String fullName;

    @Column
    String email;

    @Column
    String planName;

    @Column
    Instant expiresAt;

    @Column
    Instant createdAt;

    @Column
    Instant updatedAt;
}
