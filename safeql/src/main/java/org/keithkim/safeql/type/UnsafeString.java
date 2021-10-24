package org.keithkim.safeql.type;

import lombok.EqualsAndHashCode;

@EqualsAndHashCode
public class UnsafeString {
    private final String unsafeString;

    public UnsafeString(String unsafeString) {
        this.unsafeString = unsafeString;
    }

    public String inject() {
        return unsafeString;
    }
}
