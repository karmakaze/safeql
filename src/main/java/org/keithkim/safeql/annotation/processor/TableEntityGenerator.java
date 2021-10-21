package org.keithkim.safeql.annotation.processor;

import com.google.common.base.CaseFormat;
import com.squareup.javapoet.*;
import org.keithkim.moja.util.Tuple.Pair;
import org.keithkim.safeql.annotation.Column;
import org.keithkim.safeql.annotation.Table;
import org.keithkim.safeql.util.StringHelpers;

import javax.lang.model.element.Element;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import java.beans.ConstructorProperties;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;
import static org.keithkim.safeql.annotation.processor.ClassNames.*;
import static org.keithkim.safeql.util.StringHelpers.trimSuffix;

public class TableEntityGenerator {
    private final ClassName entityClassName;
    private final TypeElement typeElement;
    private final Table tableAnnotation;
    private final Map<String, Pair<VariableElement, Column>> fields = new LinkedHashMap<>();
    private final Map<ClassName, LinkedHashSet<Pair<VariableElement, Column>>> relations = new LinkedHashMap<>();

    public TableEntityGenerator(String qualifiedClassName, TypeElement typeElement, Table tableAnnotation) {
        String packageName = "";
        String simpleName = qualifiedClassName;
        int i = qualifiedClassName.lastIndexOf('.');
        if (i >= 0) {
            packageName = qualifiedClassName.substring(0, i);
            simpleName = qualifiedClassName.substring(i + 1);
        }
        this.entityClassName = ClassName.get(packageName, simpleName);
        this.typeElement = typeElement;
        this.tableAnnotation = tableAnnotation;
    }

    public ProcessorError addColumn(VariableElement element, Column columnAnnotation) {
        fields.put(element.getSimpleName().toString(), Pair.of(element, columnAnnotation));
        return null;
    }

    public ProcessorError addParentRelation(ClassName parentClassName, Pair<VariableElement, Column> elementAnnotation) {
        LinkedHashSet<Pair<VariableElement, Column>> childRelations = relations.get(parentClassName);
        if (childRelations == null) {
            childRelations = new LinkedHashSet<>();
            relations.put(parentClassName, childRelations);
        }
        childRelations.add(elementAnnotation);
        return null;
    }

    public void generateCode(TableTableGenerator tableTableGenerator, Processor.Context ctx) throws IOException {
        String packageName = entityClassName.packageName();
        String simpleName = entityClassName.simpleName();

        TypeName superClass = ParameterizedTypeName.get(ClassName.get("org.keithkim.safeql.schema", "Entity"), longClassName());
        TypeSpec.Builder entityClassBuilder = TypeSpec.classBuilder(simpleName)
                .addModifiers(Modifier.PUBLIC)
                .superclass(superClass);

        FieldSpec fieldSpec = FieldSpec.builder(ClassName.get("", simpleName+"Row"), "row", Modifier.PUBLIC, Modifier.FINAL).build();
        entityClassBuilder.addField(fieldSpec);

        LinkedHashMap<String, ClassName> constructorParams = new LinkedHashMap<>();
//        constructorParams.put("id", ClassName.get(Long.class));
//        constructorParams.put("accountId", ClassName.get(Long.class));
//        constructorParams.put("name", ClassName.get(String.class));
//        constructorParams.put("domain", ClassName.get(String.class));

        for (Map.Entry<String, Pair<VariableElement, Column>> me : fields.entrySet()) {
            String fieldName = me.getKey();
            Pair<VariableElement, Column> elementAnnotation = me.getValue();
            String fieldTypeString  = elementAnnotation.value1().asType().toString();
            ClassName fieldClassName = TypeNames.forTypeString(fieldTypeString, packageName);

            constructorParams.put(fieldName, fieldClassName);

//            entityClassBuilder.addMethod(MethodSpec.methodBuilder("comments")
//                .addStatement("// fieldClassName=$L, element=$L, annotation=$L",
//                        fieldName, elementAnnotation.value1(), elementAnnotation.value2()).build());

        }

        entityClassBuilder.addMethod(MethodSpec.methodBuilder("id").addModifiers(Modifier.PUBLIC)
                .returns(Long.class).addStatement("return row.id").build());

        MethodSpec.Builder toStringBuilder = MethodSpec.methodBuilder("toString")
                .addModifiers(Modifier.PUBLIC).returns(String.class)
                .addStatement("StringBuilder buffer = new StringBuilder($S)", simpleName+"{")
                .addStatement("row.intoString(buffer)");

        LinkedHashSet<Pair<VariableElement, Column>> childRelations = relations.get(entityClassName);
        if (childRelations != null) {
            for (Pair<VariableElement, Column> childRelation : childRelations) {
                VariableElement childVarElement = childRelation.value1();
                Element childOuterElement = childVarElement.getEnclosingElement();
                String childOuterSimpleName = trimSuffix(childOuterElement.getSimpleName().toString(), "Table");
                ClassName childOuterClassName = TypeNames.forTypeString(childOuterSimpleName, packageName);

                TypeName relatedFieldsType = mapTypeName(longClassName(), childOuterClassName);
                String relatedFieldsName = CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_CAMEL, childOuterSimpleName);
                String relatedFieldPlural = relatedFieldsName+"s";
                entityClassBuilder.addField(FieldSpec.builder(relatedFieldsType, relatedFieldPlural,
                                Modifier.PUBLIC, Modifier.FINAL).initializer("new $T<>()", HashMap.class).build());

                entityClassBuilder.addMethod(MethodSpec.methodBuilder(relatedFieldPlural).addModifiers(Modifier.PUBLIC)
                        .returns(linkedHashMapTypeName(longClassName(), childOuterClassName))
                        .addStatement("LinkedHashMap<$T, $T> $L = (LinkedHashMap<$T, $T>) childrenByClass.get($T.class)",
                                Long.class, childOuterClassName, relatedFieldPlural, Long.class, childOuterClassName, childOuterClassName)
                        .addStatement(StringHelpers.join("\n", asList(
                                "if ($L == null) {",
                                "  $L = new LinkedHashMap<>();",
                                "  childrenByClass.put($T.class, $L);",
                                "}")), relatedFieldPlural, relatedFieldPlural, childOuterClassName, relatedFieldPlural)
                        .addStatement("return $L", relatedFieldPlural).build());

                entityClassBuilder.addMethod(MethodSpec.methodBuilder("add"+childOuterSimpleName).addModifiers(Modifier.PUBLIC)
                    .addParameter(childOuterClassName, relatedFieldsName)
                    .addStatement("$L().put($L.row.id, $L)", relatedFieldPlural, relatedFieldsName, relatedFieldsName).build());

                toStringBuilder.addStatement("String $LString = $L().values().toString()", relatedFieldPlural, relatedFieldPlural)
                    .addStatement("buffer.append($S + $LString.substring(1, $LString.length() - 1) + $S)",
                        ", projects:[ ", relatedFieldPlural, relatedFieldPlural, " ]");
            }
        }

        toStringBuilder.addStatement("buffer.append($S)", "}")
                .addStatement("return buffer.toString()");
        entityClassBuilder.addMethod(toStringBuilder.build());

        TypeSpec tableClassTypespec = tableTableGenerator.generateCode(ctx);
        entityClassBuilder.addType(tableClassTypespec);

//    @ConstructorProperties({"id", "account_id", "name", "domain"})
//    public Project0(long id, long accountId, String name, String domain) {
//        row = new ProjectRow0(id, accountId, name, domain);
//    }

        String quotedNames = StringHelpers.join(", ", constructorParams.keySet().stream()
                .map((String s) -> StringHelpers.quote(s)).collect(Collectors.toList()));
        String unquotedNames = StringHelpers.join(", ", constructorParams.keySet());
        MethodSpec.Builder constructorBuilder = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)
            .addAnnotation(AnnotationSpec.builder(ConstructorProperties.class)
            .addMember("value", "{$L}", quotedNames).build());
        constructorParams.forEach((name, className) -> constructorBuilder.addParameter(className, name));
        constructorBuilder.addStatement("row = new $T($L)", ClassName.get(packageName, simpleName+"Row"), unquotedNames);
        entityClassBuilder.addMethod(constructorBuilder.build());

        TypeSpec typeSpec = entityClassBuilder.build();
        JavaFile.builder(packageName, typeSpec).skipJavaLangImports(true).build().writeTo(ctx.filer);
    }
}
