package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractOverlayRenderer {

    @Autowired
    protected PDFGenUtils pdfUtils;

    protected ITextComponent docComponent;

    protected ColumnText createTextColumn(PdfContentByte canvas,
                                          int boxAlignment,
                                          DocCoordinates boxCoordinates,
                                          Phrase phrase) {

        int x1 = boxCoordinates.getX();
        int y1 = boxCoordinates.getY();
        int x2 = boxCoordinates.getXPlusWidth();
        int y2 = boxCoordinates.getYPlusHeight();

        ColumnText column = new ColumnText(canvas);

        float leading = phrase.getLeading();

        column.setSimpleColumn(
                phrase,
                x1,
                y1,
                x2,
                y2,
                leading,
                boxAlignment);

        return column;
    }


    protected ColumnText createColumn(PdfContentByte canvas,
                                      Element iTextElement,
                                      DocCoordinates boxCoordinates) {

        int x1 = boxCoordinates.getX();
        int y1 = boxCoordinates.getY();
        int x2 = boxCoordinates.getXPlusWidth();
        int y2 = boxCoordinates.getYPlusHeight();

        ColumnText column = new ColumnText(canvas);

        column.setSimpleColumn(x1, y1, x2, y2);
        column.addElement(iTextElement);

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
