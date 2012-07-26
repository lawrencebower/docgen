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

    @Override
    public void renderComponent(TextComponent component, OverlayComponentRendererInfo rendererInfo) {
        drawTextBox(rendererInfo.getCanvas(), component);
    }

    private void drawTextBox(PdfContentByte canvas, TextComponent component) {

        String boxText = component.getValue();

        DocPosition position = component.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        DocCoordinates boxCoordinates = position.getCoordinates();

        drawBox(canvas,
                boxText,
                boxAlignment,
                boxCoordinates);
    }

    private void drawBox(PdfContentByte canvas,
                         String boxText,
                         int boxAlignment,
                         DocCoordinates boxCoordinates) {

        Font font = pdfUtils.getDefaultFont();
        ColumnText column = new ColumnText(canvas);
        column.setSimpleColumn(
                new Phrase(boxText, font),
                boxCoordinates.getX(),
                boxCoordinates.getY(),
                boxCoordinates.getWidth(),
                boxCoordinates.getHeight(),
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
