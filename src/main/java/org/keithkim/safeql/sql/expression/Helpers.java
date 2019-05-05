package org.keithkim.safeql.sql.expression;

import java.util.regex.Pattern;

public class Helpers {
    private static final Pattern TERM_PATTERN = Pattern.compile("[':.A-Za-z0-9_]+");
    private static final Pattern ONE_GROUP_PATTERN = Pattern.compile("[(][^()]+[)]");

    public static String grouped(String string) {
        if (ONE_GROUP_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }

    public static String group(String string) {
        if (TERM_PATTERN.matcher(string).matches() || ONE_GROUP_PATTERN.matcher(string).matches()) {
            return string;
        }
        return "(" + string + ")";
    }
}
