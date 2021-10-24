package org.keithkim.safeql.annotation;

import java.util.Collection;
import java.util.StringJoiner;

public class Utils {
    public static String quote(String s) {
        return '"' + s.replace("\"", "\\\"") + '"';
    }

    public static String join(String separator, Collection<?> items) {
        StringJoiner stringJoiner = new StringJoiner(separator);
        items.forEach(item -> stringJoiner.add(item.toString()));
        return stringJoiner.toString();
    }

    public static String trimPrefix(String s, String prefix) {
        if (s != null && s.startsWith(prefix)) {
            s = s.substring(prefix.length());
        }
        return s;
    }

    public static String trimSuffix(String s, String suffix) {
        if (s != null && s.endsWith(suffix)) {
            s = s.substring(0, s.length() - suffix.length());
        }
        return s;
    }

    public static String[] splitOnLast(String s, char c) {
        String[] ss = new String[] { "", s };
        if (s != null) {
            int i = s.lastIndexOf(c);
            if (i >= 0) {
                ss[0] = s.substring(0, i);
                ss[1] = s.substring(i + 1);
            }
        }
        return ss;
    }

    public static String beforeLast(String s, char c) {
        if (s != null) {
            int i = s.lastIndexOf(c);
            if (i >= 0) {
                s = s.substring(0, i);
            }
        }
        return s;
    }

    public static String afterLast(String s, char c) {
        if (s != null) {
            int i = s.lastIndexOf(c);
            if (i >= 0) {
                s = s.substring(i + 1);
            }
        }
        return s;
    }
}
