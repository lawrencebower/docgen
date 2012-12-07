package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.Image;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextImageComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayImageRenderer extends AbstractOverlayRenderer
        implements DocComponentRenderer<ITextImageComponent, OverlayComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextImageComponent component, OverlayComponentRendererInfo rendererInfo) {
        docComponent = component;
        PdfContentByte canvas = rendererInfo.getCanvas();
        Image iTextImage = component.createITextComponent();
        renderComponent(canvas, iTextImage);
    }

    private void renderComponent(PdfContentByte canvas, Image image) {

        DocCoordinates boxCoordinates = docComponent.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        int width = boxCoordinates.getWidth();
        int height = boxCoordinates.getHeight();

        image.scaleToFit(width, height);

        ColumnText column = createColumn(canvas,
                                         image,
                                         boxCoordinates);

        drawColumn(column);
    }

}
