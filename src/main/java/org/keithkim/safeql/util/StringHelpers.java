package org.keithkim.safeql.util;

import java.util.Optional;
import java.util.function.Function;

public class StringHelpers {
    public static String apply(Optional<String> input, Function<String, String> fn, String defaultValue) {
        if (input.isPresent()) {
            return fn.apply(input.get());
        }
        return defaultValue;
    }
}
