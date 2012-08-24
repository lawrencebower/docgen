package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextTableGenerator {

    @Autowired
    private PDFGenUtils pdfUtils;

    private PdfPTable iTextTable;

    public PdfPTable generateTable(TableComponent tableComponent) {

        makeTable(tableComponent);

        setTableWidth(tableComponent);

        setColumnWidths(tableComponent);

        mapCells(tableComponent);

        mapTableAlignment(tableComponent);

        return iTextTable;
    }

    private void mapTableAlignment(TableComponent tableComponent) {
        DocAlignment alignment = tableComponent.getPosition().getAlignment();
        int iTextAlignment = DocAlignment.mapToITextAlignment(alignment);
        iTextTable.setHorizontalAlignment(iTextAlignment);
    }

    private void makeTable(TableComponent tableComponent) {
        int columnCount = tableComponent.getColumnCount();
        iTextTable = new PdfPTable(columnCount);
    }

    private void setColumnWidths(TableComponent tableComponent) {

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

    private void setTableWidth(TableComponent tableComponent) {
        float widthPercentage = tableComponent.getWithPercentage();
        if (widthPercentage != 0) {
            iTextTable.setWidthPercentage(widthPercentage);
        }
    }

    private void mapCells(TableComponent tableComponent) {
        for (TableCell tableCell : tableComponent.getAllCells()) {

            Phrase phrase = getPhraseFromCell(tableCell);
            PdfPCell iTextCell = new PdfPCell(phrase);

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

        mapVerticalALignment(tableCell, iTextCell);

        mapHorizontalALignment(tableCell, iTextCell);
    }

    private void mapHorizontalALignment(TableCell tableCell, PdfPCell iTextCell) {

        DocAlignment horizontalAlignment = tableCell.getHorizontalAlignment();
        int iTextHorizontalAlignment = DocAlignment.mapToITextAlignment(horizontalAlignment);
        iTextCell.setHorizontalAlignment(iTextHorizontalAlignment);
    }

    private void mapVerticalALignment(TableCell tableCell, PdfPCell iTextCell) {

        DocAlignment verticalAlignment = tableCell.getVerticalAlignment();
        int iTextVerticalAlignment = DocAlignment.mapToITextAlignment(verticalAlignment);
        iTextCell.setVerticalAlignment(iTextVerticalAlignment);
    }

    private Phrase getPhraseFromCell(TableCell tableCell) {
        return pdfUtils.mapTextBlock(tableCell.getText());
    }
}