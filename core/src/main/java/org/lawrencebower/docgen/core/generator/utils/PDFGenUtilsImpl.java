package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.*;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.document.component.text.TextFragment;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.util.List;

public class PDFGenUtilsImpl implements PDFGenUtils {

    @Autowired
    private ITextTableGenerator iTextTableGenerator;

    public static final int DEFAULT_FONT_SIZE = 10;
    public static final int DEFAULT_FONT_STYLE = Font.BOLD;
    public static final int DEFAULT_FONT = Font.HELVETICA;

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
    public Font getBaseFont() {
        try {
            BaseFont baseFont = BaseFont.createFont();
            Font font = new Font(baseFont, DEFAULT_FONT_SIZE);
            return font;
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public PdfReader getPDFReaderAndUnlockForSourcePDF(String sourcePDF) {
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
        } catch (Exception e) {
            throw new DocGenException(e);
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
            if (position == null) {
                throw new DocGenException("Position is null for component " + component.getName());
            }

            DocCoordinates coordinates = position.getCoordinates();
            if (coordinates == null) {
                throw new DocGenException("Coordinates are null for component " + component.getName());
            }
        }
    }

    @Override
    public PdfPTable generateTable(TableComponent component) {
        return iTextTableGenerator.generateTable(component);
    }

    @Override
    public void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates) {
        canvas.rectangle(boxCoordinates.getX(),
                         boxCoordinates.getY(),
                         boxCoordinates.getWidth(),
                         boxCoordinates.getHeight());
        canvas.stroke();
    }

    @Override
    public Phrase mapTextBlock(TextBlock textBlock) {

        Phrase iTextPhrase = new Phrase();

        for (TextFragment textFragment : textBlock.getFragments()) {
            Font iTextFont = mapFont(textFragment.getFontInfo());
            Chunk iTextChunk = new Chunk(textFragment.getText(), iTextFont);
            iTextPhrase.add(iTextChunk);
        }

        return iTextPhrase;
    }

    private Font mapFont(FontInfo fontInfo) {

        int fontType = mapFontType(fontInfo.getFont());
        int fontStyle = mapFontStyle(fontInfo.getStyle());

        Font iTextFont = new Font(fontType,
                                  fontInfo.getFontSize(),
                                  fontStyle);

        return iTextFont;
    }

    private int mapFontType(String font) {
        return Font.HELVETICA;
    }

    private int mapFontStyle(FontStyle style) {
        switch (style){
            case BOLD : return Font.BOLD;
            case ITALIC : return Font.ITALIC;
            case PLAIN : return Font.NORMAL;
            case BOLD_ITALIC : return Font.BOLDITALIC;
            case UNDERLINE : return Font.UNDERLINE;
        }
        return Font.NORMAL;
    }
}
