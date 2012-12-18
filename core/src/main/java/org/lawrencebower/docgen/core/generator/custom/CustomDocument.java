package org.lawrencebower.docgen.core.generator.custom;

import org.lawrencebower.docgen.core.document.AbstractDocument;
import org.lawrencebower.docgen.core.document.DocType;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomDocument extends AbstractDocument<CustomComponent> {

    @Autowired
    private CustomPDFGenerator pdfGenerator;

    private CustomDocument() {//force spring creation
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
