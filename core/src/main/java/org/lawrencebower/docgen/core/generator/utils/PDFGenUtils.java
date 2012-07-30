package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Font;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.DocumentInfo;

import java.io.OutputStream;

public interface PDFGenUtils {

    void checkRequiredValuesPresent(DocumentInfo docInfo);

    Font getDefaultFont();

    float getLeading();

    PdfReader getPDFReaderForSourcePDF(String sourcePDF);

    PdfStamper getPDFStamper(PdfReader pdfReader, OutputStream pdfOutStream);

    void closePDFStamper(PdfStamper pdfStamper);
}
