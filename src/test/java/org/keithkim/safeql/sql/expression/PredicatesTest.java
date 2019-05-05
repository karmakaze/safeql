package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.keithkim.safeql.sql.expression.Predicates.*;

class PredicatesTest {
    @Test
    public void testNotFalseLiteral_returnsTrue() {
        Predicate notFalse = new Not(new Predicate("FALSE"));
        assertEquals("TRUE", notFalse.sql());
    }

    @Test
    public void testNotTrueLiteral_returnsFalse() {
        Predicate notTrue = new Not(new Predicate("true"));
        assertEquals("FALSE", notTrue.sql());
    }

    @Test
    public void testNotFalsePredicate_returnsTrue() {
        Predicate notFalse = new Not(FALSE);
        assertEquals("TRUE", notFalse.sql());
    }

    @Test
    public void testNotTruePredicate_returnsFalse() {
        Predicate notTrue = new Not(TRUE);
        assertEquals("FALSE", notTrue.sql());
    }

    @Test
    public void testNotFalseExpr_returnsTrue() {
        Predicate notFalse = new Not(And(TRUE, FALSE));
        assertEquals("TRUE", notFalse.sql());
    }

    @Test
    public void testNotTrueExpr_returnsFalse() {
        Predicate notTrue = new Not(Or(TRUE, FALSE));
        assertEquals("FALSE", notTrue.sql());
    }

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
