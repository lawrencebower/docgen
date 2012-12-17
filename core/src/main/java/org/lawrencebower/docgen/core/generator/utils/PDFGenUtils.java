package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

import java.io.OutputStream;

public interface PDFGenUtils {

    void checkRequiredValuesPresent(Document doc);

    PdfReader getPDFReaderAndUnlockForSourcePDF(String sourcePDF);

    PdfStamper getPDFStamper(PdfReader pdfReader, OutputStream pdfOutStream);

    void closePDFStamper(PdfStamper pdfStamper);

    void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates);

    Phrase mapTextBlock(TextBlock textBlock);

}
