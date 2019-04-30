package org.keithkim.typeql;

public class Cond {
    public static class EqualCol<L extends Entity, R extends Entity, T> implements SqlExpr {
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
