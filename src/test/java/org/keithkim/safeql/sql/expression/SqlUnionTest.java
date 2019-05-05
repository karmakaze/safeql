package org.keithkim.safeql.sql.expression;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.sql.expression.SqlUnion.Type;
import org.keithkim.safeql.util.System;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlUnionTest {
    @Test
    void empty() {
        SqlUnion<String> unionQuery = new SqlUnion<>();
        assertEquals("", unionQuery.sql());
    }

    @Test
    void single() {
        SqlUnion<String> unionQuery = new SqlUnion<>(
                new SqlSelect(System.Table.none, asList(System.Table.column("1"))).asTableExpr());
        assertEquals("SELECT 1", unionQuery.sql());
    }

    @Test
    void unionDefault2() {
        SqlUnion<String> unionQuery = new SqlUnion<>(
                new SqlSelect(System.Table.none, asList(System.Table.column("1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION SELECT 2", unionQuery.sql());
    }

    @Test
    void union2() {
        SqlUnion<String> unionQuery = new SqlUnion<>(Type.UNION,
                new SqlSelect(System.Table.none, asList(System.Table.column("1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION SELECT 2", unionQuery.sql());
    }

    @Test
    void unionAll2() {
        SqlUnion<String> unionQuery = new SqlUnion<>(Type.UNION_ALL,
                new SqlSelect(System.Table.none, asList(System.Table.column("1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION ALL SELECT 2", unionQuery.sql());
    }

    @Test
    void unionDistinct2() {
        SqlUnion<String> unionQuery = new SqlUnion<>(Type.UNION_DISTINCT,
                new SqlSelect(System.Table.none, asList(System.Table.column("1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION DISTINCT SELECT 2", unionQuery.sql());
    }

    @Test
    void union3() {
        SqlUnion<String> unionQuery = new SqlUnion<>(
                new SqlSelect(System.Table.none, asList(System.Table.column("1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column("2"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column("3"))).asTableExpr());
        assertEquals("SELECT 1 UNION SELECT 2 UNION SELECT 3", unionQuery.sql());
    }

    @Test
    void missingBindVars() {
        SqlUnion<String> unionQuery = new SqlUnion<>(
                new SqlSelect(System.Table.none, asList(System.Table.column(":v1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column(":v2"))).asTableExpr());
        assertEquals("SELECT :v1 UNION SELECT :v2", unionQuery.sql());
//        assertEquals(ImmutableSet.of("v1", "v2"), unionQuery.missingParams());
    }

    @Test
    void missingSomeBindVars() {
        SqlUnion<String> unionQuery = new SqlUnion<>(
                new SqlSelect(System.Table.none, asList(System.Table.column(":v1"))).asTableExpr(),
                new SqlSelect(System.Table.none, asList(System.Table.column(":v2"))).asTableExpr());
//        unionQuery.bind("v1", "value 1");
        assertEquals("SELECT :v1 UNION SELECT :v2", unionQuery.sql());
//        assertEquals(ImmutableSet.of("v2"), unionQuery.missingParams());
    }
}
