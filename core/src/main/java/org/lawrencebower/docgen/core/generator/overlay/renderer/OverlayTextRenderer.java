package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayTextRenderer extends AbstractOverlayRenderer
        implements DocComponentRenderer<TextComponent, OverlayComponentRendererInfo, Phrase> {


    @Override
    public void createAndRenderComponent(TextComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        Phrase phrase = createComponent(component);
        drawTextBox(rendererInfo.getCanvas(), phrase);
    }

    @Override
    public Phrase createComponent(TextComponent component) {
        TextBlock textBlock = ((TextComponent) docComponent).getText();
        return pdfUtils.mapTextBlock(textBlock);
    }

    private void drawTextBox(PdfContentByte canvas, Phrase phrase) {

        DocPosition position = docComponent.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        DocCoordinates boxCoordinates = position.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        ColumnText column = createColumn(canvas,
                                         phrase,
                                         boxAlignment,
                                         boxCoordinates);

        drawColumn(column);
    }

    private ColumnText createColumn(PdfContentByte canvas,
                                    Phrase phrase,
                                    int boxAlignment,
                                    DocCoordinates boxCoordinates) {

        return createTextColumn(canvas,
                                boxAlignment,
                                boxCoordinates,
                                phrase);
    }
}
