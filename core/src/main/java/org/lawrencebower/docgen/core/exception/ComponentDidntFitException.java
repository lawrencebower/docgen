package org.lawrencebower.docgen.core.exception;

public class ComponentDidntFitException extends RuntimeException {

    public ComponentDidntFitException(String message) {
        super(message);
    }

    public ComponentDidntFitException(Throwable cause) {
        super(cause);
    }

}
