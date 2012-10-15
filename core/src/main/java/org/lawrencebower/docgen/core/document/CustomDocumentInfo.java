package org.lawrencebower.docgen.core.document;

import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomDocumentInfo extends AbstractDocumentInfo {

    private CustomPDFGenerator pdfGenerator;

    public CustomDocumentInfo(CustomPDFGenerator pdfGenerator) {
        this.pdfGenerator = pdfGenerator;
    }

    @Override
    public DocType getDocType() {
        return DocType.CUSTOM;
    }

    @Override
    public PDFDocument generatePDF() {
        return pdfGenerator.generatePDF(this);
    }
}
