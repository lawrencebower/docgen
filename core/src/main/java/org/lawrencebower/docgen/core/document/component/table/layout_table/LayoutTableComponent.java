package org.lawrencebower.docgen.core.document.component.table.layout_table;

import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableCell;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class LayoutTableComponent extends AbstractTableComponent<LayoutRow, LayoutCell, LayoutHeaderCell, LayoutHeaderRow> {

    public LayoutTableComponent() {
        super(HorizontalAlignment.LEFT);
        init();
    }

    public LayoutTableComponent(String name) {
        super(HorizontalAlignment.LEFT);
        setName(name);
        init();
    }

    public LayoutTableComponent(String name, HorizontalAlignment alignment) {
        super(alignment);
        setName(name);
        init();
    }

    private void init() {
        headerRow = new LayoutHeaderRow();//empty header as default
    }

    public void setHeaderRow(LayoutHeaderCell... cells) {
        List<LayoutHeaderCell> cellList = Arrays.asList(cells);
        headerRow = new LayoutHeaderRow(cellList);
    }

    @Override
    public List<TableCell> getAllRenderableCells() {

        List<TableCell> allCells = new ArrayList<>();

        if(headerRow.isRenderHeader()){
            List<LayoutHeaderCell> headerCells = headerRow.getCells();
            allCells.addAll(headerCells);
        }

        for (LayoutRow row : rows) {
            List<LayoutCell> rowCells = row.getCells();
            allCells.addAll(rowCells);
        }

        return allCells;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.LAYOUT_TABLE;
    }
}
