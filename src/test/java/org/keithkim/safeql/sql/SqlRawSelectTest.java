package org.keithkim.safeql.sql;

import org.junit.jupiter.api.Test;
import org.keithkim.demo.photos.User;
import org.keithkim.safeql.template.Expr;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SqlRawSelectTest {
    @Test
    public void resolveSimpleReturnsStringAsExpr() {
        SqlRawSelect<User> sqlSelect = new SqlRawSelect<>("SELECT first_name, last_name FROM user");
        Expr<SqlRows<User>> resolved = sqlSelect.resolve(emptyMap());
        assertEquals("SELECT first_name, last_name FROM user", resolved.toString());
    }
}
