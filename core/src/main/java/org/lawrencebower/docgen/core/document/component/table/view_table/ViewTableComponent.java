package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableCell;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ViewTableComponent extends AbstractTableComponent<ViewRow, ViewHeaderRow> {

    public ViewTableComponent(String name) {
        super(HorizontalAlignment.LEFT);
        setName(name);
    }

    public ViewTableComponent(String name, HorizontalAlignment alignment) {
        super(alignment);
        setName(name);
    }

    @Override
    public void addRow(ViewRow row) {
        super.addRow(row);
        setCellNamesFromHeader(row);
    }

    private void setCellNamesFromHeader(ViewRow row) {

        List<ViewHeaderCell> headerCells = headerRow.getCells();

        int headerColumnCount = headerRow.getCellCount();

        if (headerColumnCount == row.getCellCount()) {
            for (int i = 0; i < headerCells.size(); i++) {

                ViewHeaderCell headerCell = headerCells.get(i);
                String columnName = headerCell.getName();

                ViewCell cell = row.getCell(i);
                cell.setName(columnName);
            }
        }
    }

    public void setHeaderRow(ViewHeaderCell... cells) {
        List<ViewHeaderCell> cellList = Arrays.asList(cells);
        headerRow = new ViewHeaderRow(cellList);
    }

    @Override
    public List<TableCell> getAllRenderableCells() {

        List<TableCell> allCells = new ArrayList<>();

        if(headerRow.isRenderHeader()){
            List<ViewHeaderCell> headerCells = headerRow.getCells();
            allCells.addAll(headerCells);
        }

        for (ViewRow row : rows) {
            List<ViewCell> rowCells = row.getCells();
            allCells.addAll(rowCells);
        }

        return allCells;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.VIEW_TABLE;
    }
}
