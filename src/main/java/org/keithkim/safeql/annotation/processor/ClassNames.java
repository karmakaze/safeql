package org.keithkim.safeql.annotation.processor;

import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.ParameterizedTypeName;
import com.squareup.javapoet.TypeName;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ClassNames {
    public static ClassName longClassName() {
        return ClassName.get(Long.class);
    }
    public static ClassName stringClassName() {
        return ClassName.get(String.class);
    }
    public static ParameterizedTypeName listTypeName(TypeName valueType) {
        return ParameterizedTypeName.get(ClassName.get(List.class), valueType);
    }
    public static ParameterizedTypeName mapTypeName(TypeName keyType, TypeName valueType) {
        return ParameterizedTypeName.get(ClassName.get(Map.class), keyType, valueType);
    }
    public static ParameterizedTypeName linkedHashMapTypeName(TypeName keyType, TypeName valueType) {
        return ParameterizedTypeName.get(ClassName.get(LinkedHashMap.class), keyType, valueType);
    }
}
