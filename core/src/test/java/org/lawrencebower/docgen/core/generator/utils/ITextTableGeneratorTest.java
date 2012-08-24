package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.pdf.PdfPTable;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.Assert.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class ITextTableGeneratorTest {

    @Autowired
    private ITextTableGenerator tableGenerator;

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

    @Test
    public void testMakeTable_validColumnWidths_noError() {
        TableComponent tableComponent = makeStandardTableComponent(3, 3);
        tableComponent.getHeaderRow().setColumnWidths(20,20,60);
        tableGenerator.generateTable(tableComponent);
    }

    @Test
    public void testMakeTable_badColumnWidths_throwsError() {
        try {
            TableComponent tableComponent = makeStandardTableComponent(3, 3);
            tableComponent.getHeaderRow().setColumnWidths(20,20,60,10);//different number of specified widths to columns
            tableGenerator.generateTable(tableComponent);
        } catch (DocGenException e) {
            assertEquals("Specified number of column widths '4' does not match the number of columns - '3'", e.getMessage());
        }
    }

    public static TableComponent makeStandardTableComponent(int colNumber, int rowNumber) {

        TableComponent tableComponent = new TableComponent("Table Name");

        TableHeaderRow headerRow = new TableHeaderRow();
        fillTableRow(headerRow, "col", colNumber);
        tableComponent.setHeaderRow(headerRow);

        for (int i = 0; i < rowNumber; i++) {
            TableRow tableRow = new TableRow();
            fillTableRow(tableRow, colNumber);
            tableComponent.addRow(tableRow);
        }

        tableComponent.setRenderBorder(true);
        tableComponent.setPosition(new DocPosition(DocAlignment.CENTER));

        return tableComponent;
    }

    private static void fillTableRow(TableRow tableRow,
                                     int colNumber) {
        fillTableRow(tableRow,
                     "",
                     colNumber);
    }

    private static void fillTableRow(TableRow tableRow,
                                     String prefix,
                                     int colNumber) {

        for (int i = 0; i < colNumber; i++) {
            tableRow.addCell(getTableCell(prefix, i));
        }

    }

    private static TableCell getTableCell(String prefix, int number) {
        return new TableCell(prefix + number);
    }
}
