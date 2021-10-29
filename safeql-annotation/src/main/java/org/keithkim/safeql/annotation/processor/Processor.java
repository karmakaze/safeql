package org.keithkim.safeql.annotation.processor;

import com.google.auto.service.AutoService;
import org.keithkim.safeql.annotation.Column;
import org.keithkim.safeql.annotation.Table;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Filer;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic;

@AutoService(Processor.class)
public class Processor extends AbstractProcessor {
    private Context ctx;

    public static class Context {
        public final Map<String, String> options;
        public final Messager messager;
        public final Filer filer;
        public final Elements elementUtils;
        public final Types typeUtils;

        Context(Map<String, String> options, Messager messager, Filer filer, Elements elementUtils, Types typeUtils) {
            this.options = options;
            this.messager = messager;
            this.filer = filer;
            this.elementUtils = elementUtils;
            this.typeUtils = typeUtils;
        }

        public void logError(String message, Element e) {
            messager.printMessage(Diagnostic.Kind.ERROR, message, e);
        }
    }

    @Override public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        ctx = new Context(processingEnv.getOptions(), processingEnv.getMessager(),
                processingEnv.getFiler(), processingEnv.getElementUtils(), processingEnv.getTypeUtils());
    }

    @Override public Set<String> getSupportedAnnotationTypes() {
        Set<String> annotations = new LinkedHashSet<String>();
        annotations.add(Table.class.getCanonicalName());
        annotations.add(Column.class.getCanonicalName());
        return annotations;
    }

    @Override public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        TableColumnProcessor tableProcessor = new TableColumnProcessor(ctx);

        tableProcessor.process(Table.class, roundEnv.getElementsAnnotatedWith(Table.class));
        tableProcessor.process(Column.class, roundEnv.getElementsAnnotatedWith(Column.class));

        tableProcessor.generate();

        return true;
    }
}
