package org.lawrencebower.docgen.core.generator.custom;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Paragraph;
import com.lowagie.text.pdf.PdfWriter;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRendererInfo;

import java.io.ByteArrayOutputStream;

public class rendererInfo implements DocComponentRendererInfo {

    private Document document;
    private boolean documentDataWritten;

    public rendererInfo() {
        makeNewDocument();
    }

    public void preparePDFWriter(ByteArrayOutputStream pdfOutStream) {
        try {
            PdfWriter.getInstance(document, pdfOutStream);
            document.open();
        } catch (DocumentException e) {
            throw new DocGenException(e);
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
