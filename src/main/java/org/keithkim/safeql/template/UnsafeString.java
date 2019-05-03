package org.keithkim.safeql.template;

import java.util.Objects;

public class UnsafeString {
    private final String unsafeString;

    public UnsafeString(String unsafeString) {
        this.unsafeString = unsafeString;
    }

    public int hashCode() {
        return unsafeString.hashCode() * 31 + 26;
    }

    public boolean equals(Object other) {
        if (other instanceof UnsafeString) {
            UnsafeString that = (UnsafeString) other;
            return Objects.equals(this.unsafeString, that.unsafeString);
        }
        return false;
    }

    public String inject() {
        return unsafeString;
    }
}
