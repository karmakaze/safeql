package org.keithkim.safeql;

import org.keithkim.safeql.sql.Entity;

public class Cond {
    public static class EqualCol<L extends Entity, R extends Entity, T> extends SqlExpr {
        public final Col<L, T> lCol;
        public final Col<R, T> rCol;

        public EqualCol(Col<L, T> lCol, Col<R, T> rCol) {
            this.lCol = lCol;
            this.rCol = rCol;
        }

        public String sql() {
            return lCol.toString() + " = " + rCol.toString();
        }

        public String toString() {
            return sql();
        }
    }
}
