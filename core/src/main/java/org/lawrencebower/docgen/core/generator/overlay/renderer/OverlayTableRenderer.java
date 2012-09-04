package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayTableRenderer
        implements DocComponentRenderer<TableComponent, OverlayComponentRendererInfo, PdfPTable> {

    @Autowired
    private PDFGenUtils pdfUtils;

    private TableComponent tableComponent;

    @Override
    public void createAndRenderComponent(TableComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.tableComponent = component;
        PdfPTable iTextTable = createComponent(tableComponent);
        drawTable(rendererInfo.getCanvas(), iTextTable);
    }

    @Override
    public PdfPTable createComponent(TableComponent component) {
        return pdfUtils.generateTable(tableComponent);
    }

    private void drawTable(PdfContentByte canvas, PdfPTable iTextTable) {

        DocPosition position = tableComponent.getPosition();

        DocCoordinates boxCoordinates = position.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        ColumnText column = createColumn(canvas,
                                         iTextTable,
                                         boxCoordinates);

        drawColumn(column);
    }

    private void renderBorderIfSet(PdfContentByte canvas,
                                   DocCoordinates boxCoordinates) {

        if (tableComponent.isRenderBorder()) {
            drawRectangle(canvas, boxCoordinates);
        }
    }

    private void drawRectangle(PdfContentByte canvas, DocCoordinates boxCoordinates) {
        pdfUtils.drawRectangle(canvas, boxCoordinates);
    }

    private ColumnText createColumn(PdfContentByte canvas,
                                    PdfPTable table,
                                    DocCoordinates boxCoordinates) {

        int x1 = boxCoordinates.getX();
        int y1 = boxCoordinates.getY();
        int x2 = boxCoordinates.getXPlusWidth();
        int y2 = boxCoordinates.getYPlusHeight();

        ColumnText column = new ColumnText(canvas);

        table.setWidthPercentage(100);

        column.setSimpleColumn(x1, y1, x2, y2);
        column.addElement(table);

        return column;
    }

    private void drawColumn(ColumnText column) {
        try {
            column.go();
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

}
