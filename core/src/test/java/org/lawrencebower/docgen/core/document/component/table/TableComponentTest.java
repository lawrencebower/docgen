package org.lawrencebower.docgen.core.document.component.table;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.*;

import java.util.List;

import static junit.framework.Assert.assertEquals;

public class TableComponentTest {

    @Test
    public void testGetColumnCount_hasColumns_returnsCorrectNumber() throws Exception {

        LayoutTableComponent component = new LayoutTableComponent("name");
        LayoutHeaderRow row = new LayoutHeaderRow();
        row.addCell(new LayoutHeaderCell());
        row.addCell(new LayoutHeaderCell());
        row.addCell(new LayoutHeaderCell());
        component.setHeaderRow(row);

        assertEquals(3, component.getColumnCount());
    }

    @Test
    public void testGetColumnCount_noCells_returnsZero() throws Exception {
        LayoutTableComponent component = new LayoutTableComponent("name");
        LayoutRow row = new LayoutRow();
        component.addRow(row);

        assertEquals(0, component.getColumnCount());
    }

    @Test
    public void testGetColumnCount_noRows_returnsZero() throws Exception {
        TableComponent component = new LayoutTableComponent("name");

        assertEquals(0, component.getColumnCount());
    }

    @Test
    public void testGetAllCells_noRows_returnsEmpty() throws Exception {
        TableComponent component = new LayoutTableComponent("name");

        assertEquals(0, component.getAllRenderableCells().size());
    }

    @Test
    public void testGetAllCells_hasCells_returnsAllCells() throws Exception {

        TableComponent component = makeTableComponentWith6Cells();

        assertEquals(9, component.getAllRenderableCells().size());
    }

    @Test
    public void testGetAllCells_hasCells_returnsAllCellsInCorrectOrder() throws Exception {

        TableComponent component = makeTableComponentWith6Cells();

        List<TableCell> allCells = component.getAllRenderableCells();
        TextComponent cellComponent = (TextComponent) allCells.get(7).getComponent();
        assertEquals("name5", cellComponent.getTextString());
    }

    @Test
    public void testSetHeaderRow_validHeader_correctCount() throws Exception {
        LayoutTableComponent component = new LayoutTableComponent("name");
        component.setHeaderRow(new LayoutHeaderCell(),
                               new LayoutHeaderCell(),
                               new LayoutHeaderCell());

        assertEquals(3, component.getHeaderRow().getColumnCount());
    }

    @Test
    public void testSetHeaderRow_validHeader_correctOrder() throws Exception {
        LayoutTableComponent component = new LayoutTableComponent("name");
        component.setHeaderRow(new LayoutHeaderCell("cell1"),
                               new LayoutHeaderCell("cell2"),
                               new LayoutHeaderCell("cell3"));

        AbstractTableHeaderRow headerRow = component.getHeaderRow();
        List<LayoutHeaderCell> headerCells = headerRow.getCells();
        TextComponent cellComponent = (TextComponent) headerCells.get(2).getComponent();
        assertEquals("cell3", cellComponent.getTextString());
    }

    @Test
    public void testGetAllRenderableCells_renderHeaderFalse_correctCellsReturned() throws Exception {

        TableComponent component = makeTableComponentWith6Cells();
        component.getHeaderRow().setRenderHeader(false);

        assertEquals(6, component.getAllRenderableCells().size());
    }

    @Test
    public void testGetAllRenderableCells_renderHeaderTrue_correctCellsReturned() throws Exception {

        TableComponent component = makeTableComponentWith6Cells();
        component.getHeaderRow().setRenderHeader(true);

        assertEquals(9, component.getAllRenderableCells().size());
    }

    private TableComponent makeTableComponentWith6Cells() {

        LayoutTableComponent component = new LayoutTableComponent("name");

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        headerRow.addCell(new LayoutHeaderCell("Col1"));
        headerRow.addCell(new LayoutHeaderCell("Col2"));
        headerRow.addCell(new LayoutHeaderCell("Col3"));

        component.setHeaderRow(headerRow);

        LayoutRow row = new LayoutRow();
        row.addCell(new LayoutCell("name1"));
        row.addCell(new LayoutCell("name2"));
        row.addCell(new LayoutCell("name3"));
        component.addRow(row);

        LayoutRow row2 = new LayoutRow();
        row2.addCell(new LayoutCell("name4"));
        row2.addCell(new LayoutCell("name5"));
        row2.addCell(new LayoutCell("name6"));
        component.addRow(row2);

        return component;
    }
}
