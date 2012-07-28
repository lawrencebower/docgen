package org.lawrencebower.docgen.core.exception;

public class DocGenException extends RuntimeException {

    public DocGenException(String message) {
        super(message);
    }

    public DocGenException(Throwable cause) {
        super(cause);
    }

}
