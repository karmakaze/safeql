package org.keithkim.safeql.util;

public class ObjectHelpers {
    public static <T> T firstNonNull(T... objects) {
        for (T object : objects) {
            if (object != null) {
                return object;
            }
        }
        return null;
    }
}
