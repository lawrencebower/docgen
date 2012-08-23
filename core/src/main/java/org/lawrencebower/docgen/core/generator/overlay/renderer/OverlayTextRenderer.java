package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayTextRenderer extends AbstractOverlayTextRenderer
        implements DocComponentRenderer<TextComponent, OverlayComponentRendererInfo> {


    @Override
    public void renderComponent(TextComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        drawTextBox(rendererInfo.getCanvas());
    }

    private void drawTextBox(PdfContentByte canvas) {

        TextBlock boxText = ((TextComponent)docComponent).getText();

        DocPosition position = docComponent.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        DocCoordinates boxCoordinates = position.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        drawBox(canvas,
                boxText,
                boxAlignment,
                boxCoordinates);
    }

    private void drawBox(PdfContentByte canvas,
                         TextBlock boxText,
                         int boxAlignment,
                         DocCoordinates boxCoordinates) {

        ColumnText column = createColumn(canvas,
                                         boxAlignment,
                                         boxCoordinates,
                                         boxText);

        drawColumn(column);
    }
}
