package org.lawrencebower.docgen.core.document;

public interface PDFGenerator<T extends DocumentInfo> {
    public PDFDocument generatePDF(T docInfo);
}
