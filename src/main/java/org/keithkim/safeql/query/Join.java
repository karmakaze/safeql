package org.keithkim.safeql.query;

import com.google.common.base.Joiner;
import org.keithkim.safeql.expression.Expr;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;
import org.keithkim.safeql.type.JoinRows;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

public class Join<L extends Entity, R extends Entity> extends Expr<JoinRows<L, R>> {
    public enum Type {
        JOIN("JOIN"),
        LEFT_JOIN("LEFT JOIN"),
        RIGHT_JOIN("RIGHT JOIN"),
        FULL_JOIN("FULL JOIN"),
        CROSS_JOIN("CROSS JOIN");

        private final String string;

        Type(String string) {
            this.string = string;
        }

        public String sql() {
            return string;
        }
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
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(equates);
    }

    public <T> Join(Table<L> left, Table.SqlColumn lCol,
                    Table<R> right, Table.SqlColumn rCol) {
        this(Type.JOIN, left, lCol, right, rCol);
    }

    public <T> Join(Type type,
                    Table<L> left, Table.SqlColumn lCol,
                    Table<R> right, Table.SqlColumn rCol) {
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol));
    }

    public <T, U> Join(Type type,
                       Table<L> left, Table<L>.SqlColumn<T> lCol, Table<L>.SqlColumn<T> lCol2,
                       Table<R> right, Table<R>.SqlColumn<T> rCol, Table<R>.SqlColumn<T> rCol2) {
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol), new Equate(lCol2, rCol2));
    }

    public <T, U, V> Join(Type type,
                          Table<L> left, Table<L>.SqlColumn<T> lCol, Table<L>.SqlColumn<U> lCol2, Table<L>.SqlColumn<V> lCol3,
                          Table<R> right, Table<R>.SqlColumn<T> rCol, Table<L>.SqlColumn<U> rCol2, Table<R>.SqlColumn<V> rCol3) {
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol), new Equate(lCol2, rCol2), new Equate(lCol3, rCol3));
    }

    public String sql() {
        return left.sql() +" "+ type.sql() +" "+ right.sql() + onClause();
    }

    protected String onClause() {
        if (type == Type.CROSS_JOIN) {
            return "";
        }
        return " ON "+ Joiner.on(" AND ").join(equates.stream().map(e -> e.sql()).collect(toList()));
    }

    public Join<L, R> where(Cond2<L, R> cond) {
        andWhere.add(cond);
        return this;
    }

    public static class Where {
    }

    public static class Cond2<L, R> {
    }

    public static class Equate<L extends Entity, R extends Entity, T> extends Expr<Boolean> {
        public final Table<L>.SqlColumn<T> lCol;
        public final Table<R>.SqlColumn<T> rCol;

        public Equate(Table<L>.SqlColumn<T> lCol, Table<R>.SqlColumn<T> rCol) {
            super(null);
            this.lCol = lCol;
            this.rCol = rCol;
        }

        public String sql() {
            return group(lCol.sql()) +" = "+ group(rCol.sql());
        }
    }
}
