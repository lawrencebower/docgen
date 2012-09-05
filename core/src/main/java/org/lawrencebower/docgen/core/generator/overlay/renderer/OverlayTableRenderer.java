package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayTableRenderer extends AbstractOverlayRenderer
        implements DocComponentRenderer<TableComponent, OverlayComponentRendererInfo, PdfPTable> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void createAndRenderComponent(TableComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        PdfPTable iTextTable = createComponent(component);
        drawTable(rendererInfo.getCanvas(), iTextTable);
    }

    @Override
    public PdfPTable createComponent(TableComponent component) {
        return pdfUtils.generateTable(component);
    }

    private void drawTable(PdfContentByte canvas, PdfPTable iTextTable) {

        DocPosition position = docComponent.getPosition();

        DocCoordinates boxCoordinates = position.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        iTextTable.setWidthPercentage(100);

        ColumnText column = createColumn(canvas,
                                         iTextTable,
                                         boxCoordinates);

        drawColumn(column);
    }

}
