package org.lawrencebower.docgen.core.document;

public interface PDFGenerator<T extends Document> {
    public PDFDocument generatePDF(T document);
}
