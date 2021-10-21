package org.keithkim.safeql.annotation.processor;

import com.squareup.javapoet.ClassName;
import org.keithkim.moja.util.Tuple.Pair;
import org.keithkim.moja.util.Tuple.Triple;
import org.keithkim.safeql.annotation.Column;
import org.keithkim.safeql.annotation.Table;

import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.util.*;

import static org.keithkim.safeql.util.StringHelpers.splitOnLast;
import static org.keithkim.safeql.util.StringHelpers.trimSuffix;

public class TableColumnProcessor {
    private final Processor.Context ctx;
    private final Map<TypeElement, LinkedHashSet<Element>> tableColumnElements = new HashMap<>();
    Map<String, Triple<TableEntityGenerator, TableTableGenerator, TableRowGenerator>>
            entityTableRows = new LinkedHashMap<>();

    TableColumnProcessor(Processor.Context ctx) {
        this.ctx = ctx;
    }

    ProcessorError process(Class<? extends Annotation> annotationClass, Set<? extends Element> elements) {
        if (Table.class.isAssignableFrom(annotationClass)) {
            for (Element tableElement : elements) {
                ProcessorError error = processTable(tableElement);
                if (error != null) {
                    return error;
                }
            }
        } else if (Column.class.isAssignableFrom(annotationClass)) {
            for (Element columnElement : elements) {
                if (columnElement instanceof VariableElement) {
                    ProcessorError error = processColumn((VariableElement) columnElement);
                    if (error != null) {
                        return error;
                    }
                } else {
                    ProcessorError error = new ProcessorError(columnElement, "@%s annotation must be on a field.",
                            Column.class.getSimpleName());
                    return error;
                }
            }
        } else {
            ProcessorError error = new ProcessorError(null, "Unsupported annotation @%s",
                    annotationClass.getSimpleName());
            ctx.logError(error.message, error.element);
            return error;
        }
        return null;
    }

    ProcessorError processTable(Element tableElement) {
        if (tableElement.getKind() != ElementKind.CLASS) {
            ProcessorError error = new ProcessorError(tableElement, "Only classes can be annotated with @%s",
                    Table.class.getSimpleName());
            ctx.logError(error.message, error.element);
            return error;
        }
        TypeElement typeElement = (TypeElement) tableElement;
        tableColumnElements.put(typeElement, new LinkedHashSet<>());

        String qualifiedName = trimSuffix(typeElement.getQualifiedName().toString(), "Table");
        Table tableAnnotation = tableElement.getAnnotation(Table.class);

        TableEntityGenerator entityGeneratedClass = new TableEntityGenerator(qualifiedName, typeElement, tableAnnotation);
        TableTableGenerator tableGeneratedClass = new TableTableGenerator(qualifiedName, typeElement, tableAnnotation);
        TableRowGenerator rowClass = new TableRowGenerator(qualifiedName + "Row", typeElement, tableAnnotation);
        Triple<TableEntityGenerator, TableTableGenerator, TableRowGenerator> triple =
                Triple.of(entityGeneratedClass, tableGeneratedClass, rowClass);
        entityTableRows.put(qualifiedName, triple);
        return null;
    }

    ProcessorError processColumn(VariableElement columnElement) {
        Element outerElement = columnElement.getEnclosingElement();
        if (outerElement.getAnnotation(Table.class) == null) {
            ProcessorError error = new ProcessorError(columnElement,
                    "Annotation @%s can only be used within a @%s annotated element.",
                    Column.class.getSimpleName(), Table.class.getSimpleName());
            ctx.logError(error.message, error.element);
            return error;
        }
        LinkedHashSet<Element> columnElements = tableColumnElements.get(outerElement);
        columnElements.add(columnElement);

        Column columnAnnotation = columnElement.getAnnotation(Column.class);
        if (columnAnnotation != null) {
            String refFullName = columnAnnotation.references();
            if (refFullName != null && !refFullName.isEmpty()) {
                String[] packageSimple = splitOnLast(trimSuffix(refFullName, "Table"), '.');
                String refPackageName = packageSimple[0];
                String refSimpleName = packageSimple[1];
                if (refPackageName.isEmpty()) {
                    refPackageName = splitOnLast(trimSuffix(outerElement.asType().toString(), "Table"), '.')[0];
                    refFullName = refPackageName +"."+ refFullName;
                }

                Triple<TableEntityGenerator, TableTableGenerator, TableRowGenerator> generators =
                        entityTableRows.get(refFullName);
                if (generators != null) {
                    TableEntityGenerator entityGenerator = generators.value1();
                    ClassName refClassName = ClassName.get(refPackageName, refSimpleName);
                    entityGenerator.addParentRelation(refClassName, Pair.of(columnElement, columnAnnotation));
                }
            }
        }
        return null;
    }

    ProcessorError generate() {
        for (Map.Entry<TypeElement, LinkedHashSet<Element>> tableColumnsEntry : tableColumnElements.entrySet()) {
            TypeElement tableElement = tableColumnsEntry.getKey();
            LinkedHashSet<Element> columnElements = tableColumnsEntry.getValue();

            Triple<TableEntityGenerator, TableTableGenerator, TableRowGenerator> generators =
                    entityTableRows.get(trimSuffix(tableElement.getQualifiedName().toString(), "Table"));
            TableEntityGenerator entityClassGenerator = generators.value1();
            TableTableGenerator tableClassGenerator = generators.value2();
            TableRowGenerator rowClassGenerator = generators.value3();

            for (Element element : tableElement.getEnclosedElements()) {
                Column columnAnnotation = element.getAnnotation(Column.class);
                if (columnAnnotation != null) {
                    if (!element.getKind().isField()) {
                        ProcessorError error = new ProcessorError(element, "Only fields can be annotated with @%s",
                                Column.class.getSimpleName());
                        ctx.logError(error.message, error.element);
                        return error;
                    }

                    entityClassGenerator.addColumn((VariableElement) element, columnAnnotation);
                    tableClassGenerator.add((VariableElement) element, columnAnnotation);
                    rowClassGenerator.add((VariableElement) element, columnAnnotation);
                }
            }

            try {
                entityClassGenerator.generateCode(tableClassGenerator, ctx);
                rowClassGenerator.generateCode(ctx);
            } catch (IOException e) {
                ProcessorError error = new ProcessorError(null, e.getMessage(), e);
                ctx.logError(error.message, error.element);
                return error;
            }
        }
        return null;
    }
}
