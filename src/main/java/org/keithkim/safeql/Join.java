package org.keithkim.safeql;

import com.google.common.base.Joiner;
import org.keithkim.safeql.sql.*;
import org.keithkim.safeql.template.Expr;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.keithkim.safeql.sql.Helpers.group;

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

        public Expr<Type> resolve() {
            return Expr.expr(string);
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

    public Expr<SqlJoinRows<L, R>> resolve(Map<String, ?> params) {
        return Expr.expr(left.resolveString(params) +" "+ type.resolve() +" "+ right.resolveString(params) + onClause(params));
    }

    protected String onClause(Map<String, ?> params) {
        if (type == Type.CROSS_JOIN) {
            return "";
        }
        return " ON "+ Joiner.on(" AND ").join(equates.stream().map(e -> e.resolveString(params)).collect(toList()));
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

        public Expr<Boolean> resolve(Map<String, ?> params) {
            String lColString = group(lCol.resolveString(params));
            String rColString = group(rCol.resolveString(params));
            return Expr.expr(lColString +" = "+ rColString);
        }

        private String colString(SqlTable<?> table, SqlTable<?>.SqlColumn<T> col, Map<String, ?> params) {
            List<String> colName = new ArrayList<>();
            table.alias().ifPresent(alias -> colName.add(alias));
            colName.add(col.resolveString(params));
            return Joiner.on(".").join(colName);
        }
    }
}
