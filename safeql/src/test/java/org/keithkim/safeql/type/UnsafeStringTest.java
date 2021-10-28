package org.keithkim.safeql.type;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.keithkim.safeql.schema.Table;
import org.keithkim.safeql.test.Project0;

import static org.junit.jupiter.api.Assertions.*;

public class UnsafeStringTest {
    @Test
    void toStringDoesNotReturnTheString() {
        UnsafeString unsafe = new UnsafeString("Robert'); DROP TABLE Students; --");

        String escapedClassName = unsafe.getClass().getCanonicalName()
                .replace(".", "\\.").replace("$", "\\$");
        assertTrue(unsafe.toString().matches("^"+ escapedClassName +"@[0-9a-f]+$"),
                "toString: "+ escapedClassName);
    }

    @Test
    void hashCodeIsStable() {
        UnsafeString unsafe1 = new UnsafeString("A");
        UnsafeString unsafe2 = new UnsafeString(new StringBuilder().append("A").toString());

        assertTrue(unsafe1.inject() != unsafe2.inject());
        assertEquals(unsafe1.hashCode(), unsafe2.hashCode());
    }

    @Test
    void hashCodeNotSameAsForString() {
        String string = "This is an arbitrary string.";
        UnsafeString unsafe = new UnsafeString(string);

        assertNotEquals(unsafe.hashCode(), string.hashCode());
    }

    @Test
    void equalsStringIsFalse() {
        String string = "A";
        UnsafeString unsafe = new UnsafeString(string);

        assertFalse(unsafe.equals(string));
    }

    @Test
    void equalsOtherEquivalentUnsafeStringIsTrue() {
        UnsafeString unsafe1 = new UnsafeString("A");
        UnsafeString unsafe2 = new UnsafeString(new StringBuilder().append("A").toString());

        assertTrue(unsafe1.inject() != unsafe2.inject());
        assertTrue(unsafe1.equals(unsafe2));
    }

    @Test
    void boxThenUnboxEqualsOriginal() {
        UnsafeString unsafe = new UnsafeString("Robert'); DROP TABLE Students; --");
        String unwrapped = unsafe.inject();

        assertEquals("Robert'); DROP TABLE Students; --", unwrapped);
    }

    @Test
    void bindingUnsafe_sql_usesunboxedValueAsBoundParameter() {
        UnsafeString unsafe = new UnsafeString("Robert'); DROP TABLE Students; --");
        Table<Project0> rawSelect = new Table<>(Project0.class,
                "SELECT id, name FROM project WHERE name = :name");

        rawSelect.bind("name", unsafe);
        String expected = String.format("SELECT id, name FROM project WHERE name = :name_%s", rawSelect.objectId);
        assertEquals(expected, rawSelect.sql());
        assertEquals(ImmutableMap.of("name_"+rawSelect.objectId, unsafe.inject()), rawSelect.localBinds());
        expected = String.format("<SQL: SELECT id, name FROM project WHERE name = :name_%s; "+
                "BIND: name_%s:Robert'); DROP TABLE Students; -->", rawSelect.objectId, rawSelect.objectId);
        assertEquals(expected, rawSelect.toString());
    }
}
