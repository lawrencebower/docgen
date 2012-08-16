package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.pdf.*;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.util.List;

public class PDFGenUtilsImpl implements PDFGenUtils {

    private static final float DEFAULT_LEADING = 9;
    private static final int DEFAULT_FONT_SIZE = 10;

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
            return new Font(baseFont, DEFAULT_FONT_SIZE);
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
            PdfReader pdfReader = new PdfReader(sourcePDF);
            unlockPdf(pdfReader);
            return pdfReader;
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    public static PdfReader unlockPdf(PdfReader reader) {
        if (reader == null) {
            return reader;
        }
        try {
            Field f = reader.getClass().getDeclaredField("encrypted");
            f.setAccessible(true);
            f.set(reader, false);
        } catch (Exception e) { // ignore
        }
        return reader;
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

    @Override
    public void checkCoordinates(List<DocComponent> components) {
        for (DocComponent component : components) {
            DocPosition position = component.getPosition();
            if(position == null){
                throw new DocGenException("Position is null for component " + component.getName());
            }

            DocCoordinates coordinates = position.getCoordinates();
            if(coordinates == null){
                throw new DocGenException("Coordinates are null for component " + component.getName());
            }
        }

    }

    @Override
    public PdfPTable generateTable(TableComponent component) {
        int columnCount = component.getColumnCount();
        PdfPTable table = new PdfPTable(columnCount);

        for (TableCell tableCell : component.getAllCells()) {
            table.addCell(tableCell.getValue());
        }

        return table;
    }

    @Override
    public void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates) {
        canvas.rectangle(boxCoordinates.getX(),
                         boxCoordinates.getY(),
                         boxCoordinates.getWidth(),
                         boxCoordinates.getHeight());
        canvas.stroke();
    }
}
