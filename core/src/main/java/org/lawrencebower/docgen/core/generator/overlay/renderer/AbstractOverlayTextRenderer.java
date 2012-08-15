package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractOverlayTextRenderer {

    @Autowired
    protected PDFGenUtils pdfUtils;

    protected DocComponent docComponent;

    protected ColumnText createColumn(PdfContentByte canvas,
                                      int boxAlignment,
                                      DocCoordinates boxCoordinates,
                                      String boxText) {

        int x1 = boxCoordinates.getX();
        int y1 = boxCoordinates.getY();
        int x2 = boxCoordinates.getXPlusWidth();
        int y2 = boxCoordinates.getYPlusHeight();

        Font font = pdfUtils.getDefaultFont();
        ColumnText column = new ColumnText(canvas);

        column.setSimpleColumn(
                new Phrase(boxText, font),
                x1,
                y1,
                x2,
                y2,
                pdfUtils.getLeading(),
                boxAlignment);

        return column;
    }

    protected void drawColumn(ColumnText column) {
        try {
            column.go();
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

    protected void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates) {
        pdfUtils.drawRectangle(canvas, boxCoordinates);
    }

    protected void renderBorderIfSet(PdfContentByte canvas,
                                     DocCoordinates boxCoordinates) {

        if (docComponent.isRenderBorder()) {
            drawRectangle(canvas, boxCoordinates);
        }
    }
}
