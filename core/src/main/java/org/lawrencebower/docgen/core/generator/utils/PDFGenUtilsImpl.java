package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.IOException;
import java.io.OutputStream;

public class PDFGenUtilsImpl implements PDFGenUtils {

    private float DEFAULT_LEADING = 9;

    @Override
    public void checkRequiredValuesPresent(DocumentInfo docInfo) {
        if (docInfo.getDocType() == null) {
            throw new DocGenException("DocInfo DocType must not be null");
        }

        if (docInfo.getComponents() == null || docInfo.getComponents().isEmpty()) {
            throw new DocGenException("DocInfo Document components are null/empty");
        }

        if (docInfo.getName() == null) {
            throw new DocGenException("DocInfo Name must not be null");
        }
    }

    @Override
    public Font getDefaultFont() {
        try {
            BaseFont baseFont = BaseFont.createFont();
            return new Font(baseFont, 10);
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public float getLeading() {
        return DEFAULT_LEADING;
    }

    @Override
    public PdfReader getPDFReaderForSourcePDF(String sourcePDF) {
        try {
            return new PdfReader(sourcePDF);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public PdfStamper getPDFStamper(PdfReader pdfReader, OutputStream pdfOutStream) {
        try {
            return new PdfStamper(pdfReader, pdfOutStream);
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public void closePDFStamper(PdfStamper pdfStamper) {
        try {
            pdfStamper.close();
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }
}
