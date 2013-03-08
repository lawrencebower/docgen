package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.exception.ComponentDidntFitException;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTableComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayTableRenderer extends AbstractOverlayRenderer
        implements DocComponentRenderer<ITextTableComponent, OverlayComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextTableComponent component, OverlayComponentRendererInfo rendererInfo) {
        docComponent = component;
        PdfContentByte canvas = rendererInfo.getCanvas();
        PdfPTable iTextTable = component.createITextComponent();
        renderComponent(canvas, iTextTable);
    }

    private void renderComponent(PdfContentByte canvas, PdfPTable iTextTable) {

        DocCoordinates boxCoordinates = docComponent.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        iTextTable.setWidthPercentage(100);

        ColumnText column = createColumn(canvas,
                                         iTextTable,
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
            String componentName = docComponent.getName();
            String message = String.format("Can not fit table into box - '%s'", componentName);
            throw new DocGenException(message);
        }
    }
}
