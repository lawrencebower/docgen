package org.lawrencebower.docgen.core.document.component.table;

import org.junit.Test;

import java.util.List;

import static junit.framework.Assert.assertEquals;

public class TableComponentTest {

    @Test
    public void testGetColumnCount_hasColumns_returnsCorrectNumber() throws Exception {

        TableComponent component = new TableComponent("name");
        TableRow row = new TableRow();
        row.addCell(new TableCell("name"));
        row.addCell(new TableCell("name"));
        row.addCell(new TableCell("name"));
        component.setHeaderRow(row);

        assertEquals(3, component.getColumnCount());
    }

    @Test
    public void testGetColumnCount_noCells_returnsZero() throws Exception {
        TableComponent component = new TableComponent("name");
        TableRow row = new TableRow();
        component.addRow(row);

        assertEquals(0, component.getColumnCount());
    }

    @Test
    public void testGetColumnCount_noRows_returnsZero() throws Exception {
        TableComponent component = new TableComponent("name");

        assertEquals(0, component.getColumnCount());
    }

    @Test
    public void testGetAllCells_noRows_returnsEmpty() throws Exception {
        TableComponent component = new TableComponent("name");

        assertEquals(0, component.getAllCells().size());
    }

    @Test
    public void testGetAllCells_hasCells_returnsAllCells() throws Exception {

        TableComponent component = makeTableComponentWith6Cells();

        assertEquals(6, component.getAllCells().size());
    }

    @Test
    public void testGetAllCells_hasCells_returnsAllCellsInCorrectOrder() throws Exception {

        TableComponent component = makeTableComponentWith6Cells();

        List<TableCell> allCells = component.getAllCells();
        assertEquals("name5", allCells.get(4).getValue());
    }

    private TableComponent makeTableComponentWith6Cells() {
        TableComponent component = new TableComponent("name");

        TableRow row = new TableRow();
        row.addCell(new TableCell("name1"));
        row.addCell(new TableCell("name2"));
        row.addCell(new TableCell("name3"));
        component.addRow(row);

        TableRow row2 = new TableRow();
        row2.addCell(new TableCell("name4"));
        row2.addCell(new TableCell("name5"));
        row2.addCell(new TableCell("name6"));
        component.addRow(row2);
        return component;
    }
}
