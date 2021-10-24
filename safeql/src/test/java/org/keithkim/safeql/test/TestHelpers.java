package org.keithkim.safeql.test;

import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class TestHelpers {
    public static void assertMatches(Pattern expectedPattern, String actual) {
        assertTrue(expectedPattern.matcher(actual).matches(),
                "Expected "+actual+"\n                                     to match "+ expectedPattern.pattern());
    }

    public static void assertMatches(String expectedPattern, String actual) {
        assertTrue(Pattern.compile(expectedPattern).matcher(actual).matches(),
                "Expected "+actual+"\n                                     to match "+ expectedPattern);
    }
}
