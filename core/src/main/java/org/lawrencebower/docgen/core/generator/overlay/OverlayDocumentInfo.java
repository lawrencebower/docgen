package org.lawrencebower.docgen.core.generator.overlay;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.document.AbstractDocumentInfo;
import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;

public class OverlayDocumentInfo extends AbstractDocumentInfo<OverlayComponent> {

    private OverlayPDFGenerator pdfGenerator;
    private String sourcePDF;

    public OverlayDocumentInfo(String name,
                               OverlayPDFGenerator pdfGenerator) {

        if(StringUtils.isWhitespace(name)){
            throw new DocGenException("Name is not set");
        }

        this.pdfGenerator = pdfGenerator;
        this.name = name;
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
