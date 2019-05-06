package org.keithkim.safeql.query;

import org.junit.jupiter.api.Test;
import org.keithkim.safeql.query.Select;
import org.keithkim.safeql.query.Union;
import org.keithkim.safeql.schema.Sys;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class UnionTest {
    @Test
    void empty() {
        Union<String> unionQuery = new Union<>();
        assertEquals("", unionQuery.sql());
    }

    @Test
    void single() {
        Union<String> unionQuery = new Union<>(
                new Select(Sys.Table.none, asList(Sys.Table.column("1"))).asTableExpr());
        assertEquals("SELECT 1", unionQuery.sql());
    }

    @Test
    void unionDefault2() {
        Union<String> unionQuery = new Union<>(
                new Select(Sys.Table.none, asList(Sys.Table.column("1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION SELECT 2", unionQuery.sql());
    }

    @Test
    void union2() {
        Union<String> unionQuery = new Union<>(Union.Type.UNION,
                new Select(Sys.Table.none, asList(Sys.Table.column("1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION SELECT 2", unionQuery.sql());
    }

    @Test
    void unionAll2() {
        Union<String> unionQuery = new Union<>(Union.Type.UNION_ALL,
                new Select(Sys.Table.none, asList(Sys.Table.column("1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION ALL SELECT 2", unionQuery.sql());
    }

    @Test
    void unionDistinct2() {
        Union<String> unionQuery = new Union<>(Union.Type.UNION_DISTINCT,
                new Select(Sys.Table.none, asList(Sys.Table.column("1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column("2"))).asTableExpr());
        assertEquals("SELECT 1 UNION DISTINCT SELECT 2", unionQuery.sql());
    }

    @Test
    void union3() {
        Union<String> unionQuery = new Union<>(
                new Select(Sys.Table.none, asList(Sys.Table.column("1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column("2"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column("3"))).asTableExpr());
        assertEquals("SELECT 1 UNION SELECT 2 UNION SELECT 3", unionQuery.sql());
    }

    @Test
    void missingBindVars() {
        Union<String> unionQuery = new Union<>(
                new Select(Sys.Table.none, asList(Sys.Table.column(":v1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column(":v2"))).asTableExpr());
        assertEquals("SELECT :v1 UNION SELECT :v2", unionQuery.sql());
//        assertEquals(ImmutableSet.of("v1", "v2"), unionQuery.missingParams());
    }

    @Test
    void missingSomeBindVars() {
        Union<String> unionQuery = new Union<>(
                new Select(Sys.Table.none, asList(Sys.Table.column(":v1"))).asTableExpr(),
                new Select(Sys.Table.none, asList(Sys.Table.column(":v2"))).asTableExpr());
//        unionQuery.bind("v1", "value 1");
        assertEquals("SELECT :v1 UNION SELECT :v2", unionQuery.sql());
//        assertEquals(ImmutableSet.of("v2"), unionQuery.missingParams());
    }
}
