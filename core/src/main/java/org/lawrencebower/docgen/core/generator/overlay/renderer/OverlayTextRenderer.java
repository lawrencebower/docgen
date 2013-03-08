package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.ComponentDidntFitException;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTextComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayTextRenderer extends AbstractOverlayRenderer<ITextTextComponent>
        implements DocComponentRenderer<ITextTextComponent, OverlayComponentRendererInfo> {


    @Override
    public void createAndRenderComponent(ITextTextComponent component, OverlayComponentRendererInfo rendererInfo) {
        docComponent = component;
        PdfContentByte canvas = rendererInfo.getCanvas();
        Phrase iTextComponent = component.createITextComponent();
        renderComponent(canvas, iTextComponent);
    }

    private void renderComponent(PdfContentByte canvas, Phrase phrase) {

        HorizontalAlignment alignment = docComponent.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);

        DocCoordinates boxCoordinates = docComponent.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        ColumnText column = createColumn(canvas,
                                         phrase,
                                         boxAlignment,
                                         boxCoordinates);

        drawColumn(column);
    }

    @Override
    protected void drawColumn(ColumnText column) {
        super.drawColumn(column);
    }

    @Override
    protected void checkIfComponentFitted(int status) {
        try {
            super.checkIfComponentFitted(status);
        } catch (ComponentDidntFitException e) {
            String stringValue = docComponent.getStringValue();
            String componentName = docComponent.getName();
            String message = String.format("Can not fit value into box - Value : '%s'\nComponent : '%s'", stringValue, componentName);
            throw new DocGenException(message);
        }
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
