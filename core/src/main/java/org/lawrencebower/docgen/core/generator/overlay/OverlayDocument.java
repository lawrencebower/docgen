package org.lawrencebower.docgen.core.generator.overlay;

import org.lawrencebower.docgen.core.document.AbstractDocument;
import org.lawrencebower.docgen.core.document.DocType;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;

public class OverlayDocument extends AbstractDocument<OverlayComponent> {

    private OverlayPDFGenerator pdfGenerator;
    private String sourcePDF;

    public OverlayDocument(String name,
                           OverlayPDFGenerator pdfGenerator) {

        super(name);
        this.pdfGenerator = pdfGenerator;
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
