package org.lawrencebower.docgen.core.document.component.table;

import java.util.ArrayList;
import java.util.List;

public class TableRow {

    private List<TableCell> cells = new ArrayList<>();

    /**
     * the column count needs to take into account the col span of cells
     */
    public int getColumnCount(){
        int colCount = 0;
        for (TableCell cell : cells) {
            colCount += cell.getColSpan();
        }

        return colCount;
    }

    public void addCell(TableCell cell) {
        cells.add(cell);
    }

    public void setCells(List<TableCell> cells) {
        this.cells = cells;
    }

    public List<TableCell> getCells() {
        return cells;
    }
}
