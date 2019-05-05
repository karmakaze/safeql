package org.keithkim.safeql;

import com.google.common.base.Joiner;
import org.keithkim.safeql.sql.expression.Expr;
import org.keithkim.safeql.sql.expression.SqlEntity;
import org.keithkim.safeql.sql.expression.SqlJoinRows;
import org.keithkim.safeql.sql.expression.SqlTable;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.keithkim.safeql.sql.expression.Helpers.group;

public class Join<L extends SqlEntity, R extends SqlEntity> extends Expr<SqlJoinRows<L, R>> {
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
    protected final SqlTable left;
    protected final SqlTable right;
    protected final List<Equate<L, R, ?>> equates;
    protected final List<Cond2<L, R>> andWhere = new ArrayList<>();

    public Join(SqlTable<L> left, SqlTable<R> right, Equate<L, R, ?>... equates) {
        this(Type.JOIN, left, right, equates);
    }

    public Join(Type type, SqlTable<L> left, SqlTable<R> right, Equate<L, R, ?>... equates) {
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(equates);
    }

    public <T> Join(SqlTable<L> left, SqlTable.SqlColumn lCol,
                    SqlTable<R> right, SqlTable.SqlColumn rCol) {
        this(Type.JOIN, left, lCol, right, rCol);
    }

    public <T> Join(Type type,
                    SqlTable<L> left, SqlTable.SqlColumn lCol,
                    SqlTable<R> right, SqlTable.SqlColumn rCol) {
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol));
    }

    public <T, U> Join(Type type,
                       SqlTable<L> left, SqlTable<L>.SqlColumn<T> lCol, SqlTable<L>.SqlColumn<T> lCol2,
                       SqlTable<R> right, SqlTable<R>.SqlColumn<T> rCol, SqlTable<R>.SqlColumn<T> rCol2) {
        super(null);
        this.type = type;
        this.left = left;
        this.right = right;
        this.equates = asList(new Equate(lCol, rCol), new Equate(lCol2, rCol2));
    }

    public <T, U, V> Join(Type type,
                          SqlTable<L> left, SqlTable<L>.SqlColumn<T> lCol, SqlTable<L>.SqlColumn<U> lCol2, SqlTable<L>.SqlColumn<V> lCol3,
                          SqlTable<R> right, SqlTable<R>.SqlColumn<T> rCol, SqlTable<L>.SqlColumn<U> rCol2, SqlTable<R>.SqlColumn<V> rCol3) {
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

    public static class Equate<L extends SqlEntity, R extends SqlEntity, T> extends Expr<Boolean> {
        public final SqlTable<L>.SqlColumn<T> lCol;
        public final SqlTable<R>.SqlColumn<T> rCol;

        public Equate(SqlTable<L>.SqlColumn<T> lCol, SqlTable<R>.SqlColumn<T> rCol) {
            super(null);
            this.lCol = lCol;
            this.rCol = rCol;
        }

        public String sql() {
            return group(lCol.sql()) +" = "+ group(rCol.sql());
        }
    }
}
