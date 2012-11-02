package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextCheckBoxComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayCheckBoxRenderer
        extends AbstractOverlayRenderer
        implements DocComponentRenderer<ITextCheckBoxComponent, OverlayComponentRendererInfo> {

    private static final int FONT_SIZE = 10;//todo maybe work out font size based on box area

    @Override
    public void createAndRenderComponent(ITextCheckBoxComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        Phrase phrase = component.createITextComponent();
        renderComponent(rendererInfo.getCanvas(), phrase);
    }

    private void renderComponent(PdfContentByte canvas, Phrase phrase) {

        HorizontalAlignment alignment = docComponent.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);

        DocCoordinates boxCoordinates = docComponent.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        drawBox(canvas,
                phrase,
                boxAlignment,
                boxCoordinates);
    }

    private void drawBox(PdfContentByte canvas,
                         Phrase phrase,
                         int boxAlignment,
                         DocCoordinates boxCoordinates) {

        ColumnText column = createTextColumn(canvas,
                                             boxAlignment,
                                             boxCoordinates,
                                             phrase);

        /**
         * for checkbox - the leading is overridden to be the font size
         */
        column.setLeading(FONT_SIZE);

        drawColumn(column);
    }


}
