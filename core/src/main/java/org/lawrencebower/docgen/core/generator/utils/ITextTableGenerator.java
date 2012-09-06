package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomComponentRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextTableGenerator {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Autowired
    private CustomComponentRenderer componentRenderer;

    public PdfPTable generateTable(TableComponent tableComponent) {

        PdfPTable iTextTable = makeTable(tableComponent);

        setTableWidth(tableComponent, iTextTable);

        setColumnWidths(tableComponent, iTextTable);

        mapCells(tableComponent, iTextTable);

        mapTableAlignment(tableComponent, iTextTable);

        return iTextTable;
    }

    private void mapTableAlignment(TableComponent tableComponent, PdfPTable iTextTable) {
        HorizontalAlignment alignment = tableComponent.getPosition().getHorizontalAlignment();
        int iTextAlignment = HorizontalAlignment.mapToITextAlignment(alignment);
        iTextTable.setHorizontalAlignment(iTextAlignment);
    }

    private PdfPTable makeTable(TableComponent tableComponent) {
        int columnCount = tableComponent.getColumnCount();
        return new PdfPTable(columnCount);
    }

    private void setColumnWidths(TableComponent tableComponent, PdfPTable iTextTable) {

        int[] relativeWidths = tableComponent.getColumnWidths();

        if (relativeWidths.length == 0) {
            return;
        }

        try {
            iTextTable.setWidths(relativeWidths);
        } catch (DocumentException e) {
            throw new DocGenException(e);
        }
    }

    private void setTableWidth(TableComponent tableComponent, PdfPTable iTextTable) {
        float widthPercentage = tableComponent.getWithPercentage();
        if (widthPercentage != 0) {
            iTextTable.setWidthPercentage(widthPercentage);
        }
    }

    private void mapCells(TableComponent tableComponent, PdfPTable iTextTable) {

        for (TableCell tableCell : tableComponent.getAllCells()) {

            PdfPCell iTextCell = new PdfPCell();

            Element element = processCell(tableCell);

            iTextCell.addElement(element);

            mapCellAlignment(tableCell, iTextCell);

            setCellColor(tableCell, iTextCell);

            setCellPadding(tableCell, iTextCell);

            setCellBorder(tableComponent.isRenderBorder(),
                          iTextCell);

            iTextTable.addCell(iTextCell);
        }
    }

    private void setCellBorder(boolean renderBorder,
                               PdfPCell iTextCell) {

        if (renderBorder) {
            iTextCell.setBorder(Rectangle.LEFT +
                                Rectangle.RIGHT +
                                Rectangle.TOP +
                                Rectangle.BOTTOM);
        } else {
            iTextCell.setBorder(0);
        }
    }

    private void setCellPadding(TableCell tableCell, PdfPCell iTextCell) {
        iTextCell.setPadding(tableCell.getPadding());
    }

    private void setCellColor(TableCell tableCell, PdfPCell iTextCell) {
        if (tableCell.hasBackgroundColor()) {
            iTextCell.setBackgroundColor(tableCell.getBackgroundColor());
        }
    }

    private void mapCellAlignment(TableCell tableCell, PdfPCell iTextCell) {

        /**
         * horizontal alignment ignored by table cells when adding content with
         * addElement() - uses the horizontal alignment of the nested component
         */
        mapVerticalAlignment(tableCell, iTextCell);

    }

    private void mapVerticalAlignment(TableCell tableCell, PdfPCell iTextCell) {

        VerticalAlignment verticalAlignment = tableCell.getVerticalAlignment();
        int iTextVerticalAlignment = VerticalAlignment.mapToITextAlignment(verticalAlignment);
        iTextCell.setVerticalAlignment(iTextVerticalAlignment);
    }

    private Element processCell(TableCell tableCell) {
        return componentRenderer.createComponent(tableCell.getComponent());
    }
}
