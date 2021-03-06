package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFGenerator;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayOutputStream;

public abstract class AbstractPDFGenerator<T extends Document> implements PDFGenerator<T> {

    protected PDFGenUtils pdfGenUtils;
    protected ByteArrayOutputStream pdfOutStream;

    @Autowired
    public void setPdfGenUtils(PDFGenUtils pdfGenUtils) {
        this.pdfGenUtils = pdfGenUtils;
    }

    protected void resetPDFOutputStream() {
        this.pdfOutStream = new ByteArrayOutputStream();
    }

    public ByteArrayOutputStream getPdfOutStream() {
        return pdfOutStream;
    }

    protected PDFDocumentImpl getPDFFromPDFStream() {

        byte[] bytes = pdfOutStream.toByteArray();

        return new PDFDocumentImpl(bytes);
    }

    protected abstract void checkRequiredValuesPresent();

}
