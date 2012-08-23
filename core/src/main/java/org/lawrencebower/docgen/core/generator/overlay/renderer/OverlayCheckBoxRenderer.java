package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayCheckBoxRenderer extends AbstractOverlayTextRenderer
        implements DocComponentRenderer<CheckBoxComponent, OverlayComponentRendererInfo> {

    @Override
    public void renderComponent(CheckBoxComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        drawTextBox(rendererInfo.getCanvas());
    }

    private void drawTextBox(PdfContentByte canvas) {

        boolean isSelected = ((CheckBoxComponent) docComponent).getSelected();

        DocPosition position = docComponent.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        DocCoordinates boxCoordinates = position.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        drawBox(canvas,
                isSelected,
                boxAlignment,
                boxCoordinates);
    }

    private void drawBox(PdfContentByte canvas,
                         boolean isSelected,
                         int boxAlignment,
                         DocCoordinates boxCoordinates) {

        String boxText = getTextFromSelected(isSelected);
        TextBlock textBlock = new TextBlock(boxText);

        ColumnText column = createColumn(canvas,
                                         boxAlignment,
                                         boxCoordinates,
                                         textBlock);

        drawColumn(column);
    }

    private String getTextFromSelected(boolean selected) {
        if(selected){
            return "X";
        }
        return "";
    }

}
