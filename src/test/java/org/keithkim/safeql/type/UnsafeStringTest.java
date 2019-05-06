package org.keithkim.safeql.type;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.type.UnsafeString;

import static org.junit.jupiter.api.Assertions.*;

public class UnsafeStringTest {
    @Test
    public void toStringDoesNotReturnTheString() {
        UnsafeString unsafe = new UnsafeString("Robert'); DROP TABLE Students; --");

        String escapedClassName = unsafe.getClass().getCanonicalName()
                .replace(".", "\\.").replace("$", "\\$");
        assertTrue(unsafe.toString().matches("^"+ escapedClassName +"@[0-9a-f]+$"),
                "toString: "+ escapedClassName);
    }

    @Test
    public void hashCodeIsStable() {
        UnsafeString unsafe1 = new UnsafeString("A");
        UnsafeString unsafe2 = new UnsafeString(new StringBuilder().append("A").toString());

        assertTrue(unsafe1.inject() != unsafe2.inject());
        assertEquals(unsafe1.hashCode(), unsafe2.hashCode());
    }

    @Test
    public void hashCodeNotSameAsForString() {
        String string = "This is an arbitrary string.";
        UnsafeString unsafe = new UnsafeString(string);

        assertNotEquals(unsafe.hashCode(), string.hashCode());
    }

    @Test
    public void equalsStringIsFalse() {
        String string = "A";
        UnsafeString unsafe = new UnsafeString(string);

        assertFalse(unsafe.equals(string));
    }

    @Test
    public void equalsOtherEquivalentUnsafeStringIsTrue() {
        UnsafeString unsafe1 = new UnsafeString("A");
        UnsafeString unsafe2 = new UnsafeString(new StringBuilder().append("A").toString());

        assertTrue(unsafe1.inject() != unsafe2.inject());
        assertTrue(unsafe1.equals(unsafe2));
    }

    @Test
    public void boxThenUnboxEqualsOriginal() {
        UnsafeString unsafe = new UnsafeString("Robert'); DROP TABLE Students; --");
        String unwrapped = unsafe.inject();

        assertEquals("Robert'); DROP TABLE Students; --", unwrapped);
    }
}
