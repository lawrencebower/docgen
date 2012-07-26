package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.DocumentInfo;

public interface PDFGenerator<T extends DocumentInfo> {
    public PDFDocument generatePDF(T docInfo);
}
