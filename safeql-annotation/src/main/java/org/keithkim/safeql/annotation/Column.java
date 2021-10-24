package org.keithkim.safeql.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.FIELD) @Retention(RetentionPolicy.CLASS)
public @interface Column {
    /**
     * Override default column name (all lower-cased class name with underscores).
     */
    String name() default "";

    /**
     * Mark as PRIMARY_KEY or ALLOW_NULL.
     */
    ColumnOption[] options() default {};

    /**
     * Mark foreign key references.
     */
    String references() default "";
}
