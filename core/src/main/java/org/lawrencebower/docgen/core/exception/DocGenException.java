package org.lawrencebower.docgen.core.exception;

public class DocGenException extends RuntimeException {
    public DocGenException() {
        super();
    }

    public DocGenException(String message) {
        super(message);
    }

    public DocGenException(String message, Throwable cause) {
        super(message, cause);
    }

    public DocGenException(Throwable cause) {
        super(cause);
    }

    protected DocGenException(String message,
                              Throwable cause,
                              boolean enableSuppression,
                              boolean writableStackTrace) {

        super(message, cause, enableSuppression, writableStackTrace);
    }
}
