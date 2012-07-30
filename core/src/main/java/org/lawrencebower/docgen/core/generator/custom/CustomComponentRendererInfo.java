package org.lawrencebower.docgen.core.generator.custom;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Paragraph;
import com.lowagie.text.pdf.PdfWriter;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRendererInfo;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.io.ByteArrayOutputStream;

public class CustomComponentRendererInfo implements DocComponentRendererInfo {

    private Document document;
    private boolean documentDataWritten;

    public CustomComponentRendererInfo() {
        makeNewDocument();
    }

    /**
     * access to Document is controlled by this class -
     * this class records whether data has been written
     */
    private Document getDocument() {
        return document;
    }

    public void preparePDFWriter(ByteArrayOutputStream pdfOutStream) {
        try {
            checkOutStreamValid(pdfOutStream);
            PdfWriter.getInstance(document, pdfOutStream);
            document.open();
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

    private void checkOutStreamValid(ByteArrayOutputStream pdfOutStream) throws DocumentException {
        if(pdfOutStream == null){
            throw new DocumentException("Null outputStream");
        }
    }

    public void makeNewDocument() {
        this.document = new Document();
    }

    public void closeDocument() {
        if (documentDataWritten) {
            document.close();
        }
    }

    public void addToDocument(Paragraph paragraph) {
        try {
            checkAndOpenDocument();
            document.add(paragraph);
            documentDataWritten = true;
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

    private void checkAndOpenDocument() {
        if (!document.isOpen()) {
            document.open();
        }
    }
}
