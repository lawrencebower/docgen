package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayTextRenderer implements DocComponentRenderer<TextComponent, OverlayComponentRendererInfo> {

    @Autowired
    private PDFGenUtils pdfUtils;

    private TextComponent textComponent;

    @Override
    public void renderComponent(TextComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.textComponent = component;
        drawTextBox(rendererInfo.getCanvas());
    }

    private void drawTextBox(PdfContentByte canvas) {

        String boxText = textComponent.getValue();

        DocPosition position = textComponent.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        DocCoordinates boxCoordinates = position.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        drawBox(canvas,
                boxText,
                boxAlignment,
                boxCoordinates);
    }

    private void renderBorderIfSet(PdfContentByte canvas,
                                   DocCoordinates boxCoordinates) {

        if (textComponent.isRenderBorder()) {
            drawRectangle(canvas, boxCoordinates);
        }
    }

    private void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates) {
        pdfUtils.drawRectangle(canvas, boxCoordinates);
    }

    private void drawBox(PdfContentByte canvas,
                         String boxText,
                         int boxAlignment,
                         DocCoordinates boxCoordinates) {

        Font font = pdfUtils.getDefaultFont();
        ColumnText column = new ColumnText(canvas);

        int x1 = boxCoordinates.getX();
        int y1 = boxCoordinates.getY();
        int x2 = boxCoordinates.getXPlusWidth();
        int y2 = boxCoordinates.getYPlusHeight();

        column.setSimpleColumn(
                new Phrase(boxText, font),
                x1,
                y1,
                x2,
                y2,
                pdfUtils.getLeading(),
                boxAlignment);

        drawColumn(column);
    }

    private void drawColumn(ColumnText column) {
        try {
            column.go();
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

}
