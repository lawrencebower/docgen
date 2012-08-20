package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.pdf.PdfPTable;
import org.junit.Before;
import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableRow;

import static junit.framework.Assert.assertEquals;

public class ITextTableGeneratorTest {

    private ITextTableGenerator tableGenerator;

    @Before
    public void setup() {
        this.tableGenerator = new ITextTableGenerator();
    }

    @Test
    public void testMakeTable_standardComponent_returnsValidRowCount() {

        TableComponent tableComponent = makeStandardTableComponent(3, 3);
        PdfPTable iTextTable = tableGenerator.generateTable(tableComponent);
        assertEquals(4, iTextTable.getRows().size());//includes header
    }

    @Test
    public void testMakeTable_standardComponent_returnsValidColCount() {

        TableComponent tableComponent = makeStandardTableComponent(3, 3);
        PdfPTable iTextTable = tableGenerator.generateTable(tableComponent);
        assertEquals(3, iTextTable.getNumberOfColumns());
    }

    @Test
    public void testMakeTable_setWidthPercentage_successfullySet() {

        TableComponent tableComponent = makeStandardTableComponent(3, 3);
        tableComponent.setWidthPercentage(12.5f);
        PdfPTable iTextTable = tableGenerator.generateTable(tableComponent);
        assertEquals(12.5f, iTextTable.getWidthPercentage());
    }

    public static TableComponent makeStandardTableComponent(int colNumber, int rowNumber) {

        TableComponent tableComponent = new TableComponent("Table Name");

        tableComponent.setHeaderRow(getTableRow("col", colNumber));

        for (int i = 0; i < rowNumber; i++) {
            tableComponent.addRow(getTableRow(colNumber));
        }

        return tableComponent;
    }

    private static TableRow getTableRow(int colNumber) {
        return getTableRow("", colNumber);
    }

    private static TableRow getTableRow(String prefix, int colNumber) {

        TableRow tableRow = new TableRow();

        for (int i = 0; i < colNumber; i++) {
            tableRow.addCell(getTableCell(prefix, i));
        }

        return tableRow;
    }

    private static TableCell getTableCell(String prefix, int number) {
        return new TableCell(prefix + number);
    }
}
