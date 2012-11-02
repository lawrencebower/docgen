package org.lawrencebower.docgen.core.generator.custom;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.PageSize;
import com.lowagie.text.pdf.PdfWriter;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.document.DocComponentRendererInfo;

import java.io.OutputStream;

public class CustomComponentRendererInfo implements DocComponentRendererInfo {

    protected Document document;
    protected boolean documentDataWritten;

    public CustomComponentRendererInfo(OutputStream pdfOutStream) {
        makeNewDocument();
        preparePDFWriter(pdfOutStream);
    }

    /**
     * access to Document is controlled by this class -
     * this class records whether data has been written
     */
    private Document getDocument() {
        return document;
    }

    private void preparePDFWriter(OutputStream pdfOutStream) {
        try {
            checkOutStreamValid(pdfOutStream);
            PdfWriter instance = PdfWriter.getInstance(document, pdfOutStream);
            instance.setCompressionLevel(0);
            checkAndOpenDocument();
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

    private void checkOutStreamValid(OutputStream pdfOutStream) throws DocumentException {
        if(pdfOutStream == null){
            throw new DocumentException("Null outputStream");
        }
    }

    private void makeNewDocument() {
        this.document = new Document(PageSize.A4);//todo paramaterize page size
    }

    public void closeDocument() {
        try {
            if (documentDataWritten) {
                document.close();
            }
        } catch (Exception e) {
            throw new DocGenException(e);
        }
    }

    public void addToDocument(Element element) {
        try {
            checkAndOpenDocument();
            document.add(element);
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
