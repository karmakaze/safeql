package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.predicate.Predicates.*;
import static org.keithkim.safeql.predicate.Predicates.Or;

public class AndTest {
    @Test
    public void testAndFalseFalse_isFalse() {
        Predicate and = And(FALSE, FALSE);
        assertEquals("FALSE", and.sql());
    }

    @Test
    public void testAndFalseTrue_isFalse() {
        Predicate and = And(FALSE, TRUE);
        assertEquals("FALSE", and.sql());
    }

    @Test
    public void testAndTrueFalse_isFalse() {
        Predicate and = And(TRUE, FALSE);
        assertEquals("FALSE", and.sql());
    }

    @Test
    public void testAndTrueTrue_isTrue() {
        Predicate and = And(TRUE, TRUE);
        assertEquals("TRUE", and.sql());
    }

    @Test
    public void testAndTrueFalseExpr_isFalse() {
        Predicate or = And(Or(TRUE, TRUE), Or(FALSE, FALSE));
        assertEquals("FALSE", or.sql());
    }
    @Test
    public void testAndTrueTrueExpr_isFalse() {
        Predicate or = And(Or(TRUE, TRUE), Or(TRUE, TRUE));
        assertEquals("TRUE", or.sql());
    }
}
