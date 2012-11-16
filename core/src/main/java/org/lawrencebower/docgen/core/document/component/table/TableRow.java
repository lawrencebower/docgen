package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public class TableRow {

    private List<TableCell> cells = new ArrayList<>();

    /**
     * the column count needs to take into account the col span of cells
     */
    public int getColumnCount() {
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

    public TableCell getCell(int colNum) {

        if (cells.size() <= colNum) {
            String messageTemplate = "Cant retrieve cell number '%s' from table - only has %s cells";
            int actualRowNum = cells.size();
            String message = String.format(messageTemplate, colNum, actualRowNum);
            throw new DocGenException(message);
        }

        return cells.get(colNum);
    }
}
