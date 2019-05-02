package org.keithkim.typeql;

import com.google.common.base.Joiner;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;

public class Join<L extends Entity, R extends Entity> extends SqlExpr {
    public enum Type {
        JOIN,
        LEFT_JOIN,
        RIGHT_JOIN,
        FULL_JOIN,
        CROSS_JOIN,
    }

    protected final Type type;
    protected final Table left;
    protected final Table right;
    protected final List<Equate<L, R, ?>> equates;
    protected final List<Cond2<L, R>> andWhere = new ArrayList<>();

    public Join(Table<L> left, Table<R> right, Equate<L, R, ?>... equates) {
        this(Type.JOIN, left, right, equates);
    }

    public Join(Type type, Table<L> left, Table<R> right, Equate<L, R, ?>... equates) {
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(equates);
    }

    public <T> Join(Table<L> left, Col<L, T> lCol,
                    Table<R> right, Col<R, T> rCol) {
        this(Type.JOIN, left, lCol, right, rCol);
    }

    public <T> Join(Type type,
                    Table<L> left, Col<L, T> lCol,
                    Table<R> right, Col<R, T> rCol) {
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol));
    }

    public <T, U> Join(Type type,
                       Table<L> left, Col<L, T> lCol, Col<L, U> lCol2,
                       Table<R> right, Col<R, T> rCol, Col<R, U> rCol2) {
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol), new Equate(lCol2, rCol2));
    }

    public <T, U, V> Join(Type type,
                          Table<L> left, Col<L, T> lCol, Col<L, U> lCol2, Col<L, V> lCol3,
                          Table<R> right, Col<R, T> rCol, Col<R, U> rCol2, Col<R, V> rCol3) {
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol), new Equate(lCol2, rCol2), new Equate(lCol3, rCol3));
    }

    public String sql() {
        return left +" "+ type.name().replace("_", " ") +" "+ right + onClause();
    }

    protected String onClause() {
        if (type == Type.CROSS_JOIN) {
            return "";
        }
        return " ON "+ Joiner.on(" AND ").join(equates);
    }

    public Join where(Cond2<L, R> cond) {
        andWhere.add(cond);
        return this;
    }

    public static class Where {
    }

    public static class Cond2<L, R> {
    }

    public static class Equate<L extends Entity, R extends Entity, T> extends SqlExpr {
        public final Col<L, T> lCol;
        public final Col<R, T> rCol;

        public Equate(Col<L, T> lCol, Col<R, T> rCol) {
            this.lCol = lCol;
            this.rCol = rCol;
        }
        public String sql() {
            return lCol.sql() +" = "+ rCol.sql();
        }
        public String toString() {
            return sql();
        }
    }
}
