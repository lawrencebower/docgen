package org.lawrencebower.docgen.core.document;

import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayDocumentInfo extends AbstractDocumentInfo {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;
    private String sourcePDF;

    public OverlayDocumentInfo() {
    }

    public void setSourcePDF(String sourcePDF) {
        this.sourcePDF = sourcePDF;
    }

    public String getSourcePDF() {
        return sourcePDF;
    }

    @Override
    public DocType getDocType() {
        return DocType.OVERLAY;
    }

    @Override
    public PDFDocument generatePDF() {
        return pdfGenerator.generatePDF(this);
    }
}
