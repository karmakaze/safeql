package org.keithkim.safeql.sample.projects;

import org.keithkim.safeql.annotation.Column;
import org.keithkim.safeql.annotation.Table;

import java.time.Instant;

import static org.keithkim.safeql.annotation.ColumnOption.ALLOW_NULL;
import static org.keithkim.safeql.annotation.ColumnOption.PRIMARY_KEY;

@Table
public abstract class ProjectTable {
    @Column(options = {PRIMARY_KEY})
    Long id;

    @Column(references = "Account")
    Long accountId;

    @Column
    String name;

    @Column(options = ALLOW_NULL)
    String domain;

    @Column
    Instant createdAt;

    @Column
    Instant updatedAt;
}
