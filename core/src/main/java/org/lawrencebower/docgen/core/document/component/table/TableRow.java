package org.lawrencebower.docgen.core.document.component.table;

import java.util.ArrayList;
import java.util.List;

public class TableRow {

    private List<TableCell> cells = new ArrayList<>();

    public int getColumnCount(){
        return cells.size();
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
