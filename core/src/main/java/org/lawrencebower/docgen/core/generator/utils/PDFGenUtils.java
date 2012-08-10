package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Font;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;

import java.io.OutputStream;
import java.util.List;

public interface PDFGenUtils {

    void checkRequiredValuesPresent(DocumentInfo docInfo);

    Font getDefaultFont();

    float getLeading();

    PdfReader getPDFReaderForSourcePDF(String sourcePDF);

    PdfStamper getPDFStamper(PdfReader pdfReader, OutputStream pdfOutStream);

    void closePDFStamper(PdfStamper pdfStamper);

    void checkCoordinates(List<DocComponent> components);

    PdfPTable generateTable(TableComponent component);

    void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates);
}
