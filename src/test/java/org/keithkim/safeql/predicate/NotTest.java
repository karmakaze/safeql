package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.predicate.Predicates.*;

public class NotTest {
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
}
