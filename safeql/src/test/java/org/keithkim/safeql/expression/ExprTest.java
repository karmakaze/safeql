package org.keithkim.safeql.expression;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.*;
import static org.keithkim.safeql.test.TestHelpers.assertMatches;

class ExprTest {
    @Test
    void null_equals_returnsFalse() {
        Expr<String> a = new Expr<>("SELECT * FROM account");

        assertFalse(a.equals(null));
    }

    @Test
    void incompatibleType_equals_returnsFalse() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        String unrelated = "SELECT * FROM account";

        assertFalse(a.equals(unrelated));
    }

    @Test
    void sameButDistinctIdentity_equals_returnsTrue() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        Expr<String> b = new Expr<>(new StringBuilder("SELECT *").append(" FROM account").toString());

        assertEquals(a, b);
    }

    @Test
    void identicallyConstructed_equals_returnsTrue() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        Expr<String> b = new Expr<>("SELECT * FROM account");

        assertEquals(a, b);
    }

    @Test
    void different_equals_returnsFalse() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        Expr<String> b = new Expr<>("SELECT * FROM project");

        assertNotEquals(a, b);
    }

    @Test
    void sameButDistinctIdentity_hashCode_match() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        Expr<String> b = new Expr<>(new StringBuilder("SELECT *").append(" FROM account").toString());

        assertEquals(a.hashCode(), b.hashCode());
    }

    @Test
    void identicallyConstructed_hashCode_match() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        Expr<String> b = new Expr<>("SELECT * FROM account");

        assertEquals(a.hashCode(), b.hashCode());
    }

    @Test
    void different_hashCode_dontMatch() {
        Expr<String> a = new Expr<>("SELECT * FROM account");
        Expr<String> b = new Expr<>("SELECT * FROM project");

        assertNotEquals(a.hashCode(), b.hashCode());
    }

    @Test
    void nobinds_sql_returnsLiteral() {
        Expr<String> subject = new Expr<>("SELECT * FROM account");

        assertEquals("SELECT * FROM account", subject.sql());
    }

    @Test
    void withBinds_sql_returnsLiteral() {
        Expr<String> subject = new Expr<>("SELECT * FROM account WHERE id BETWEEN :min_id AND :max_id LIMIT :limit OFFSET :offset");
        subject.bind("min_id", 1000);
        subject.bind("max_id", 2000);
        subject.bind("limit", 10);
        subject.bind("offset", 5);

        String expected = String.format("SELECT * FROM account WHERE id BETWEEN :min_id_%s AND :max_id_%s "+
                        "LIMIT :limit_%s OFFSET :offset_%s", subject.objectId, subject.objectId,
                                                             subject.objectId, subject.objectId);
        assertEquals(expected, subject.sql());
    }

    @Test
    void nobinds_binds_returnsEmptyMap() {
        Expr<String> subject = new Expr<>("SELECT * FROM account");

        assertEquals(emptyMap(), subject.localBinds());
    }

    @Test
    void withMultiplebinds_binds_returnsSortedMapWithAllbinds() {
        Expr<String> subject = new Expr<>("SELECT * FROM account WHERE id BETWEEN :min_id AND :max_id LIMIT :limit OFFSET :offset");
        subject.bind("min_id", 1000);
        subject.bind("max_id", 2000);
        subject.bind("limit", 10);
        subject.bind("offset", 5);

        assertTrue(subject.localBinds() instanceof LinkedHashMap);
        Map<String, Integer> expected = ImmutableMap.of("min_id_"+subject.objectId, 1000,
                "max_id_"+subject.objectId, 2000,
                "limit_"+subject.objectId, 10,
                "offset_"+subject.objectId, 5);
        assertEquals(expected, subject.localBinds());
    }

    @Test
    void bindNamed_again_ignored() {
        Expr<String> subject = Expr.expr("SELECT * FROM account WHERE id = :id");
        subject.bind("id", 1);
        String expected = String.format("<SQL: SELECT * FROM account WHERE id = :id_%s; BIND: id_%s:1>",
                subject.objectId, subject.objectId);
        assertEquals(expected, subject.toString());

        subject.bind("id", 2);
        assertEquals(expected, subject.toString());
    }

    @Test
    void bindLocalPositional_shouldSetNamedParam() {
        Expr<String> subject = Expr.expr("SELECT * FROM account WHERE id = ?", 1000);
        String varName = "_1_" + subject.objectId;

        assertEquals("<SQL: SELECT * FROM account WHERE id = :"+varName+"; BIND: "+varName+":1000>",
                subject.toString());
    }

    @Test
    void bindLocalPositionals_shouldSetNamedParams() {
        Expr<String> subject = Expr.expr("SELECT * FROM point WHERE (x, y) IN ((?, ?), (?, ?))",
                1, 2, 3, 4);
        Map<String, Object> binds = new LinkedHashMap<>(4);
        for (int i = 1; i <= 4; i++) {
            binds.put("_"+ i +"_" + subject.objectId, i);
        }

        String sql = String.format("SELECT * FROM point WHERE (x, y) IN ((:%s, :%s), (:%s, :%s))", binds.keySet().toArray());
        String bind = Joiner.on(", ").withKeyValueSeparator(":").join(binds);
        assertEquals("<SQL: "+ sql +"; BIND: "+bind+">", subject.toString());
    }

    @Test
    void bindPositional_again_ignored() {
        Expr<String> subject = Expr.expr("SELECT * FROM account WHERE id = ?");
        subject.bind("?", 1);
        String expected = String.format("<SQL: SELECT * FROM account WHERE id = :_1_%s; BIND: _1_%s:1>",
                subject.objectId, subject.objectId);
        assertEquals(expected, subject.toString());

        subject.bind("?", 2);
        assertEquals(expected, subject.toString());
    }

    @Test
    void nobinds_toString_shouldFormatInAngleQuotes() {
        Expr<String> subject = new Expr<>("SELECT * FROM account");

        assertEquals("<SQL: SELECT * FROM account;>", subject.toString());
    }

    @Test
    void withOneBinding_toString_shouldFormatInAngleQuotes() {
        Expr<String> subject = new Expr<>("SELECT * FROM account WHERE id >= :min_id");
        subject.bind("min_id", 1000);

        String expected = String.format("<SQL: SELECT * FROM account WHERE id >= :min_id_%s; BIND: min_id_%s:1000>",
                subject.objectId, subject.objectId);
        assertEquals(expected, subject.toString());
    }

    @Test
    void withMultiplebinds_toString_shouldFormatSortedInAngleQuotes() {
        Expr<String> subject = new Expr<>("SELECT * FROM account WHERE id BETWEEN :min_id AND :max_id LIMIT :limit OFFSET :offset");
        subject.bind("min_id", 1000);
        subject.bind("max_id", 2000);
        subject.bind("limit", 10);
        subject.bind("offset", 5);

        String expected = String.format("<SQL: SELECT * FROM account WHERE id BETWEEN :min_id_%s AND :max_id_%s "+
                "LIMIT :limit_%s OFFSET :offset_%s; BIND: min_id_%s:1000, max_id_%s:2000, limit_%s:10, offset_%s:5>",
                subject.objectId, subject.objectId, subject.objectId, subject.objectId,
                subject.objectId, subject.objectId, subject.objectId, subject.objectId);
        assertEquals(expected, subject.toString());
    }
}
