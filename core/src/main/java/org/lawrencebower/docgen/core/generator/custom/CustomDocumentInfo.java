package org.lawrencebower.docgen.core.generator.custom;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.document.AbstractDocumentInfo;
import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.document.PDFDocument;

public class CustomDocumentInfo extends AbstractDocumentInfo<CustomComponent> {

    private CustomPDFGenerator pdfGenerator;

    public CustomDocumentInfo(String name,
                              CustomPDFGenerator pdfGenerator) {

        if(StringUtils.isWhitespace(name)){
            throw new DocGenException("Name is not set");
        }

        this.pdfGenerator = pdfGenerator;
        this.name = name;
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
