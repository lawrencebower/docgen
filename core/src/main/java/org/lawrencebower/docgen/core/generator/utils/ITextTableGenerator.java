package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;

public class ITextTableGenerator {

    private PdfPTable iTextTable;

    public PdfPTable generateTable(TableComponent tableComponent) {

        makeTable(tableComponent);

        setTableWidth(tableComponent);

        mapCells(tableComponent);

        return iTextTable;
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

            iTextTable.addCell(iTextCell);
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
        return new Phrase(tableCell.getValue());
    }

    private void makeTable(TableComponent tableComponent) {
        int columnCount = tableComponent.getColumnCount();
        iTextTable = new PdfPTable(columnCount);
    }
}
