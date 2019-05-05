package org.keithkim.safeql.sql.expression;

import java.util.regex.Pattern;

public class Helpers {
    private static final Pattern TERM_PATTERN = Pattern.compile("[':.A-Za-z0-9_]+");

    public static String group(String string) {
        if (TERM_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }
}
