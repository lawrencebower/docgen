package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TableComponent extends DocComponent {

    private TableRow headerRow = new TableRow();
    private List<TableRow> rows = new ArrayList<>();

    public TableComponent(String name) {
        super(name);
    }

    public TableComponent(String name, DocPosition position) {
        super(name, position);
    }

    public int getColumnCount() {
        return headerRow.getColumnCount();
    }

    public void setHeaderRow(TableRow row) {
        this.headerRow = row;
    }

    public void setHeaderRow(TableCell... cells) {
        this.headerRow = new TableRow();
        this.headerRow.setCells(Arrays.asList(cells));
    }

    public TableRow getHeaderRow() {
        return headerRow;
    }

    public void addRow(TableRow row) {
        this.rows.add(row);
    }

    public List<TableCell> getAllCells() {
        List<TableCell> allCells = new ArrayList<>();

        allCells.addAll(headerRow.getCells());

        for (TableRow row : rows) {
            allCells.addAll(row.getCells());
        }

        return allCells;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TABLE;
    }

}
