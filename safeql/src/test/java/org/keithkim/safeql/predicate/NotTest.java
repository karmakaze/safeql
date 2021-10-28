package org.keithkim.safeql.predicate;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.expression.LiteralExpr;

import static java.util.Collections.emptySet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.keithkim.safeql.predicate.Predicates.*;

public class NotTest {
    @Test
    public void testNotFalseLiteral_returnsTrue() {
        class MyBooleanLiteral extends LiteralExpr<Boolean> implements Predicate {
            public MyBooleanLiteral(String sql) {
                super(sql, emptySet());
            }
        };

        Predicate notFalse = new Not(new MyBooleanLiteral("FALSE"));
        assertEquals("TRUE", notFalse.sql());
    }

    @Test
    public void testNotTrueLiteral_returnsFalse() {
        Predicate notTrue = new Not(TRUE);
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
        Predicate notFalse = new Not(AND(TRUE, FALSE));
        assertEquals("TRUE", notFalse.sql());
    }

    @Test
    public void testNotTrueExpr_returnsFalse() {
        Predicate notTrue = new Not(OR(TRUE, FALSE));
        assertEquals("FALSE", notTrue.sql());
    }
}
