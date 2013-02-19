package org.lawrencebower.docgen.core.generator.model.itext_component.utils;

import com.lowagie.text.*;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponentFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;
import java.util.List;
import java.util.List;

public class ITextTableGenerator {

    @Autowired
    private ITextComponentFactory componentFactory;

    public PdfPTable generateTable(TableComponent<? extends TableRow, ? extends TableHeaderRow> tableComponent) {

        PdfPTable iTextTable = makeTable(tableComponent);

        setTableWidth(tableComponent, iTextTable);

        setColumnWidths(tableComponent, iTextTable);

        mapCells(tableComponent, iTextTable);

        mapTableAlignment(tableComponent, iTextTable);

        return iTextTable;
    }

    private void mapTableAlignment(TableComponent tableComponent, PdfPTable iTextTable) {
        HorizontalAlignment alignment = tableComponent.getAlignment();
        int iTextAlignment = HorizontalAlignment.mapToITextAlignment(alignment);
        iTextTable.setHorizontalAlignment(iTextAlignment);
    }

    private PdfPTable makeTable(TableComponent tableComponent) {
        int columnCount = tableComponent.getColumnCount();
        return new PdfPTable(columnCount);
    }

    private void setColumnWidths(TableComponent tableComponent, PdfPTable iTextTable) {
        try {
            int[] relativeWidths = tableComponent.getColumnWidths();

            if (relativeWidths.length != 0) {
                iTextTable.setWidths(relativeWidths);
            }
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

    private void mapCells(TableComponent<? extends TableRow, ? extends TableHeaderRow> tableComponent,
                          PdfPTable iTextTable) {

        List<TableCell> allRenderableCells = tableComponent.getAllRenderableCells();

        for (TableCell tableCell : allRenderableCells) {

            PdfPCell iTextCell = processCell(tableCell);

            mapCellAlignment(tableCell, iTextCell);

            mapCellSpans(tableCell, iTextCell);

            setCellColor(tableCell, iTextCell);

            setCellPadding(tableComponent, tableCell, iTextCell);

            boolean renderBorder = tableComponent.isRenderBorder();
            setCellBorder(renderBorder, iTextCell);

            iTextTable.addCell(iTextCell);
        }
    }

    private void mapCellSpans(TableCell tableCell, PdfPCell iTextCell) {

        int rowSpan = tableCell.getRowSpan();
        int colSpan = tableCell.getColSpan();

        iTextCell.setRowspan(rowSpan);
        iTextCell.setColspan(colSpan);
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

    private void setCellPadding(TableComponent tableComponent,
                                TableCell tableCell,
                                PdfPCell iTextCell) {

        float padding = tableComponent.getTablePadding();

        /**
         * cell padding over rides table padding
         */
        if (tableCell.getPadding() != -1) {
            padding = tableCell.getPadding();
        }

        iTextCell.setPadding(padding);
    }

    private void setCellColor(TableCell tableCell, PdfPCell iTextCell) {
        if (tableCell.hasBackgroundColor()) {
            Color backgroundColor = tableCell.getBackgroundColor();
            iTextCell.setBackgroundColor(backgroundColor);
        }
    }

    private void mapCellAlignment(TableCell tableCell, PdfPCell iTextCell) {

        mapVerticalAlignment(tableCell, iTextCell);

        mapHorizontalAlignment(tableCell, iTextCell);
    }

    private void mapVerticalAlignment(TableCell tableCell, PdfPCell iTextCell) {

        VerticalAlignment verticalAlignment = tableCell.getVerticalAlignment();
        int iTextVerticalAlignment = VerticalAlignment.mapToITextAlignment(verticalAlignment);
        iTextCell.setVerticalAlignment(iTextVerticalAlignment);
    }

    /**
     * sets the alignment on the ITextTable cell. The horizontal alignment will be ignored for all nested table
     * components except TableTextComponent. Components other than TableTextComponent should have their
     * HorizontalAlignment set.
     */
    private void mapHorizontalAlignment(TableCell tableCell, PdfPCell iTextCell) {

        DocComponent component = getCellComponent(tableCell);
        HorizontalAlignment horizontalAlignment = component.getAlignment();

        int iTextHorizontalAlignment = HorizontalAlignment.mapToITextAlignment(horizontalAlignment);
        iTextCell.setHorizontalAlignment(iTextHorizontalAlignment);
    }

    /**
     * For TableText to be dealt with, the constructor arg needs to be used, otherwise all other element types
     * use the addElement() method
     */
    private PdfPCell processCell(TableCell tableCell) {

        DocComponent component = getCellComponent(tableCell);

        ITextComponent iTextComponent = componentFactory.createComponent(component);

        Element iTextElement = iTextComponent.createITextComponent();

        PdfPCell iTextCell;

        if (component.getComponentType() == DocComponentType.TABLE_TEXT) {
            iTextCell = new PdfPCell((Phrase) iTextElement);
        } else if(component.getComponentType() == DocComponentType.IMAGE){
            Image image = (Image) iTextElement;
            ImageComponent imageComponent = (ImageComponent) component;
            boolean fitToTable = imageComponent.isFitToTable();
            iTextCell = new PdfPCell(image, fitToTable);
        } else {
            iTextCell = new PdfPCell();
            iTextCell.addElement(iTextElement);
        }

        return iTextCell;
    }

    private DocComponent getCellComponent(TableCell tableCell) {

        if(tableCell.getComponent() == null){
            String message = String.format("TableCell '%s' has null component", tableCell);
            throw new DocGenException(message);
        }

        return tableCell.getComponent();
    }
}
