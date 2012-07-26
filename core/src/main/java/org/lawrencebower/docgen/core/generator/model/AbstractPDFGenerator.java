package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtilsImpl;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayOutputStream;

public abstract class AbstractPDFGenerator<T extends DocumentInfo> implements PDFGenerator<T> {

    protected PDFGenUtilsImpl pdfGenUtils;
    protected ByteArrayOutputStream pdfOutStream;

    @Autowired
    public void setPdfGenUtils(PDFGenUtilsImpl pdfGenUtils) {
        this.pdfGenUtils = pdfGenUtils;
    }

    protected void resetPDFOutputStream() {
        this.pdfOutStream = new ByteArrayOutputStream();
    }

    protected PDFDocumentImpl getPDFFromPDFStream() {

        byte[] bytes = pdfOutStream.toByteArray();

        return new PDFDocumentImpl(bytes);
    }

    protected abstract void checkRequiredValuesPresent();

}
