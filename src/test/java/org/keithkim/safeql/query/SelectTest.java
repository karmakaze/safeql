package org.keithkim.safeql.query;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.quicklog.Account;
import org.keithkim.demo.quicklog.Project;
import org.keithkim.safeql.schema.Entity;
import org.keithkim.safeql.schema.Table;

import java.beans.ConstructorProperties;
import java.util.LinkedHashMap;
import java.util.Map;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SelectTest {
    @Test
    void simpleTest() {
        Project.Table projectTable = new Project.Table("project", null);

        Select<Project> selectQuery = new Select<>(projectTable, asList(projectTable.idCol, projectTable.nameCol));
        assertEquals("SELECT project.id, project.name FROM project", selectQuery.sql());
    }

    @Test
    void nestedTest() {
        Table<Abc> subQuery1 = new Table<>(Abc.class, "SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6", "t1");

        Table<Account> subQuery2 = new Table<>(Account.class, "SELECT * FROM ?", "t2");
        subQuery2.bindLocal("?", subQuery1);

        Select<Account> selectFromT2 = new Select(subQuery2);

        assertEquals("SELECT t2.* FROM (SELECT * FROM (SELECT 1 a, 2 b, 3 c UNION ALL SELECT 4, 5, 6) t1) t2",
                selectFromT2.sql());
    }

    public static class Abc extends Entity<Long> {
        public Long a;
        public Long b;
        public Long c;

        @ConstructorProperties({"a", "b", "c"})
        public Abc(long a, long b, long c) {
            this.a = a;
            this.b = b;
            this.c = c;
        }

        @Override
        public Abc withId(Long a) {
            return new Abc(a, b, c);
        }

        @Override
        public Map<String, ?> attributes() {
            LinkedHashMap<String, Object> attributes = new LinkedHashMap<>(3);
            attributes.put("a", a);
            attributes.put("b", b);
            attributes.put("c", c);
            return attributes;
        }
    }
}
