package org.keithkim.safeql.annotation.processor;

import com.squareup.javapoet.*;
import org.keithkim.moja.util.Tuple.Pair;
import org.keithkim.safeql.annotation.Column;
import org.keithkim.safeql.annotation.Table;

import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import java.io.IOException;
import java.util.*;

public class TableRowGenerator {
    private final ClassName tableRowClassName;
    private final TypeElement typeElement;
    private final Table tableAnnotation;
    private final Map<String, Pair<VariableElement, Column>> fields = new LinkedHashMap<>();

    public TableRowGenerator(String qualifiedClassName, TypeElement typeElement, Table tableAnnotation) {
        String packageName = "";
        String simpleName = qualifiedClassName;
        int i = qualifiedClassName.lastIndexOf('.');
        if (i >= 0) {
            packageName = qualifiedClassName.substring(0, i);
            simpleName = qualifiedClassName.substring(i + 1);
        }
        this.tableRowClassName = ClassName.get(packageName, simpleName);
        this.typeElement = typeElement;
        this.tableAnnotation = tableAnnotation;
    }

    public ProcessorError add(VariableElement element, Column columnAnnotation) {
        fields.put(element.getSimpleName().toString(), Pair.of(element, columnAnnotation));
        return null;
    }

    public void generateCode(Processor.Context ctx) throws IOException {
        String packageName = tableRowClassName.packageName();
        String simpleName = tableRowClassName.simpleName();

        TypeSpec.Builder tableRowClassBuilder = TypeSpec.classBuilder(simpleName).addModifiers(Modifier.PUBLIC);
        MethodSpec.Builder constructorBuilder = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC);

        StringJoiner formatString = new StringJoiner(", ");
        StringJoiner fieldNames = new StringJoiner(", ");

        for (Map.Entry<String, Pair<VariableElement, Column>> me : fields.entrySet()) {
            String fieldName = me.getKey();
            Pair<VariableElement, Column> elementAnnotation = me.getValue();

            String fieldPackageName = packageName;
            String fieldClassName = elementAnnotation.value1().asType().toString();
            int i = fieldClassName.lastIndexOf('.');
            if (i >= 1) {
                fieldPackageName = fieldClassName.substring(0, i);
                fieldClassName = fieldClassName.substring(i + 1);
            }
            ClassName fieldType = ClassName.get(fieldPackageName, fieldClassName);
            FieldSpec fieldSpec = FieldSpec.builder(fieldType, fieldName, Modifier.PUBLIC).build();
            tableRowClassBuilder.addField(fieldSpec);

            constructorBuilder.addParameter(fieldType, fieldName);
            constructorBuilder.addStatement("this.$N = $N", fieldName, fieldName);

            formatString.add(fieldName+":%s");
            fieldNames.add(fieldName);
        }

        TypeSpec typeSpec = tableRowClassBuilder
                .addMethod(constructorBuilder.build())
                .addMethod(MethodSpec.methodBuilder("toString")
                        .addModifiers(Modifier.PUBLIC).returns(String.class)
                        .addStatement("return $S + intoString(new StringBuilder()) + $S", simpleName+"{", "}")
                        .build())
                .addMethod(MethodSpec.methodBuilder("intoString")
                        .addModifiers(Modifier.PUBLIC).returns(StringBuilder.class)
                        .addParameter(StringBuilder.class, "buffer")
                        .addStatement("buffer.append(String.format($S,$L$L))", formatString, "\n", fieldNames)
                        .addStatement("return buffer")
                        .build())
                .build();

        JavaFile.builder(packageName, typeSpec).skipJavaLangImports(true).build().writeTo(ctx.filer);
    }
}
