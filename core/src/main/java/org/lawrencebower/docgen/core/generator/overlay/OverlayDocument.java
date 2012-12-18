package org.lawrencebower.docgen.core.generator.overlay;

import org.lawrencebower.docgen.core.document.AbstractDocument;
import org.lawrencebower.docgen.core.document.DocType;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayDocument extends AbstractDocument<OverlayComponent> {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;
    private String sourcePDF;

    private OverlayDocument() {//force spring creation
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
