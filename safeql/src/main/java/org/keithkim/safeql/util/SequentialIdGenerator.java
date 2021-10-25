package org.keithkim.safeql.util;

import java.util.concurrent.atomic.AtomicLong;

import static java.lang.Character.MAX_RADIX;

public class SequentialIdGenerator {
    private final AtomicLong lastId = new AtomicLong();

    public String makeIdString() {
        long id = lastId.incrementAndGet();
        return Long.toString(id, MAX_RADIX);
    }

    public long makeIdInteger() {
        return lastId.incrementAndGet();
    }
}
