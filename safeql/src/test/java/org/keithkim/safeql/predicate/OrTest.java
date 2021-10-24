package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.predicate.Predicates.*;

public class OrTest {
    @Test
    public void testOrFalseFalse_isFalse() {
        Predicate or = OR(FALSE, FALSE);
        assertEquals("FALSE", or.sql());
    }

    @Test
    public void testOrFalseTrue_isFalse() {
        Predicate or = OR(FALSE, TRUE);
        assertEquals("TRUE", or.sql());
    }

    @Test
    public void testORTrueFalse_isFalse() {
        Predicate or = OR(TRUE, FALSE);
        assertEquals("TRUE", or.sql());
    }

    @Test
    public void testOrTrueTrue_isTrue() {
        Predicate or = OR(TRUE, TRUE);
        assertEquals("TRUE", or.sql());
    }

    @Test
    public void testOrFalseFalseExpr_isTrue() {
        Predicate or = OR(AND(FALSE, FALSE), AND(FALSE, FALSE));
        assertEquals("FALSE", or.sql());
    }

    @Test
    public void testOrTrueFalseExpr_isTrue() {
        Predicate or = OR(AND(TRUE, TRUE), AND(FALSE, FALSE));
        assertEquals("TRUE", or.sql());
    }
}
