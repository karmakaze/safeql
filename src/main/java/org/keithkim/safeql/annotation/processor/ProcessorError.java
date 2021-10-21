package org.keithkim.safeql.annotation.processor;

import javax.lang.model.element.Element;

public class ProcessorError {
    public final String message;
    public final Element element;

    public ProcessorError(Element element, String msg, Object... args) {
        this.message = String.format(msg, args);
        this.element = element;
    }
}
