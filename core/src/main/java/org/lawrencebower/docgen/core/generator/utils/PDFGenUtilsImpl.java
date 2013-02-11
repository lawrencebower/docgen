package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.document.component.text.TextFragment;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;

public class PDFGenUtilsImpl implements PDFGenUtils {

    @Override
    public void checkRequiredValuesPresent(Document doc) {
        if (doc.getDocType() == null) {
            throw new DocGenException("Document DocType must not be null");
        }

        if ((doc.getComponents() == null) || doc.getComponents().isEmpty()) {
            throw new DocGenException("Document Document components are null/empty");
        }

        if (doc.getName() == null) {
            throw new DocGenException("Document Name must not be null");
        }
    }

    @Override
    public PdfReader getPDFReaderAndUnlockForSourcePDF(InputStream sourcePDF) {
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

            if(textFragment.getText() == null){
                continue;
            }

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
