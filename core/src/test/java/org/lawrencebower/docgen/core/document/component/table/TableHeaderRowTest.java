package org.lawrencebower.docgen.core.document.component.table;

import org.junit.Test;
import org.lawrencebower.docgen.core.exception.DocGenException;

import static junit.framework.Assert.assertEquals;

public class TableHeaderRowTest {

    @Test
    public void testSetColumnWidths_validWidths_setsSuccessfully() throws Exception {
        TableHeaderRow tableHeaderRow = new TableHeaderRow();
        tableHeaderRow.addCell(new TableCell("value1"));
        tableHeaderRow.addCell(new TableCell("value2"));
        tableHeaderRow.setColumnWidths(50, 50);
        assertEquals(2, tableHeaderRow.getColumnWidths().length);
    }

    @Test
    public void testAddCell_widthNotSet_existingWidthsCleared() throws Exception {
        TableHeaderRow tableHeaderRow = new TableHeaderRow();
        tableHeaderRow.addCell(new TableCell("value1"));
        tableHeaderRow.addCell(new TableCell("value2"));
        tableHeaderRow.setColumnWidths(50, 50);
        tableHeaderRow.addCell(new TableCell("value3"));//this will clear existing widths
        assertEquals(0, tableHeaderRow.getColumnWidths().length);
    }

    @Test
    public void testSetColumnWidths_invalidWidths_throwsError() throws Exception {
        TableHeaderRow tableHeaderRow = new TableHeaderRow();
        tableHeaderRow.addCell(new TableCell("value1"));
        tableHeaderRow.addCell(new TableCell("value2"));

        try {
            tableHeaderRow.setColumnWidths(50, 50, 50);//too many columns
        } catch (DocGenException e) {
            String message = "Specified number of column widths '3' does not match the number of columns - '2'";
            assertEquals(message, e.getMessage());
        }
    }
}
