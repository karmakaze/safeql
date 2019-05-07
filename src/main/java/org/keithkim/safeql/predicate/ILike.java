package org.keithkim.safeql.predicate;

import lombok.EqualsAndHashCode;
import org.keithkim.safeql.expression.Expr;

@EqualsAndHashCode(callSuper = true)
public class ILike extends BinaryPredicate<String> {
    private final Expr<String> subject;
    private final Expr<String> pattern;

    public ILike(Expr<String> subject, Expr<String> pattern) {
        super(null, "ILIKE", null);
        this.subject = subject;
        this.pattern = pattern;
    }

    @Override
    public void bind(String name, Object value) {
        subject.bind(name, value);
        pattern.bind(name, value);
    }

    public String sql() {
        return group(subject.sql()) +" ILIKE "+ group(pattern.sql());
    }
}
