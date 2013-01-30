package org.lawrencebower.docgen.core.generator.overlay;

import org.lawrencebower.docgen.core.document.AbstractDocument;
import org.lawrencebower.docgen.core.document.DocType;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;

public class OverlayDocument extends AbstractDocument<OverlayComponent> {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;
    private Resource sourcePDF;

    private OverlayDocument() {//force spring creation
    }

    public void setSourcePDF(Resource sourcePDF) {
        this.sourcePDF = sourcePDF;
    }

    public Resource getSourcePDF() {
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
