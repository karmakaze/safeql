package org.keithkim.safeql.annotation.processor;

import com.squareup.javapoet.ClassName;

import static org.keithkim.safeql.annotation.Utils.splitOnLast;

public class TypeNames {
    public static ClassName forTypeString(String typeString, String defaultPackageName) {
        ClassName className = null;
        if (typeString != null) {
            String[] packageSimple = splitOnLast(typeString, '.');
            String packageName = packageSimple[0];
            String simpleName = packageSimple[1];
            if (packageName.isEmpty()) {
                packageName = defaultPackageName;
            }
            className = ClassName.get(packageName, simpleName);
        }
        return className;
    }
}
