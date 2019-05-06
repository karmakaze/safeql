package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.predicate.Predicates.*;

public class OrTest {
    @Test
    public void testOrFalseFalse_isFalse() {
        Predicate or = Or(FALSE, FALSE);
        assertEquals("FALSE", or.sql());
    }

    @Test
    public void testOrFalseTrue_isFalse() {
        Predicate or = Or(FALSE, TRUE);
        assertEquals("TRUE", or.sql());
    }

    @Test
    public void testORTrueFalse_isFalse() {
        Predicate or = Or(TRUE, FALSE);
        assertEquals("TRUE", or.sql());
    }

    @Test
    public void testOrTrueTrue_isTrue() {
        Predicate or = Or(TRUE, TRUE);
        assertEquals("TRUE", or.sql());
    }

    @Test
    public void testOrFalseFalseExpr_isTrue() {
        Predicate or = Or(And(FALSE, FALSE), And(FALSE, FALSE));
        assertEquals("FALSE", or.sql());
    }

    @Test
    public void testOrTrueFalseExpr_isTrue() {
        Predicate or = Or(And(TRUE, TRUE), And(FALSE, FALSE));
        assertEquals("TRUE", or.sql());
    }
}
