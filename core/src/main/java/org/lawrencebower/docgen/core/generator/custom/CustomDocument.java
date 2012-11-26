package org.lawrencebower.docgen.core.generator.custom;

import org.lawrencebower.docgen.core.document.AbstractDocument;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;

public class CustomDocument extends AbstractDocument<CustomComponent> {

    private CustomPDFGenerator pdfGenerator;

    public CustomDocument(String name,
                          CustomPDFGenerator pdfGenerator) {
        super(name);
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
