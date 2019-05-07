package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.predicate.Predicates.*;

public class AndTest {
    @Test
    public void testAndFalseFalse_isFalse() {
        Predicate and = AND(FALSE, FALSE);
        assertEquals("FALSE", and.sql());
    }

    @Test
    public void testAndFalseTrue_isFalse() {
        Predicate and = AND(FALSE, TRUE);
        assertEquals("FALSE", and.sql());
    }

    @Test
    public void testAndTrueFalse_isFalse() {
        Predicate and = AND(TRUE, FALSE);
        assertEquals("FALSE", and.sql());
    }

    @Test
    public void testAndTrueTrue_isTrue() {
        Predicate and = AND(TRUE, TRUE);
        assertEquals("TRUE", and.sql());
    }

    @Test
    public void testAndTrueFalseExpr_isFalse() {
        Predicate or = AND(OR(TRUE, TRUE), OR(FALSE, FALSE));
        assertEquals("FALSE", or.sql());
    }
    @Test
    public void testAndTrueTrueExpr_isFalse() {
        Predicate or = AND(OR(TRUE, TRUE), OR(TRUE, TRUE));
        assertEquals("TRUE", or.sql());
    }
}
