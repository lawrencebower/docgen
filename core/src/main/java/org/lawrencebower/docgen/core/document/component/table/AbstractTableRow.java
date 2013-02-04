package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractTableRow<T extends TableCell> implements TableRow<T> {

    protected List<T> cells = new ArrayList<>();

    protected AbstractTableRow() {
    }

    protected AbstractTableRow(List<T> cells) {
        this.cells = cells;
    }

    /**
     * the column count needs to take into account the col span of cells
     */
    @Override
    public int getColumnCount() {
        int colCount = 0;
        for (T cell : cells) {
            colCount += cell.getColSpan();
        }

        return colCount;
    }

    @Override
    public int getCellCount() {
        return cells.size();
    }

    @Override
    public List<T> getCells() {
        return cells;
    }

    public void setCells(List<T> cells) {
        this.cells.clear();
        this.cells.addAll(cells);
    }


    @Override
    public void addCell(T cell) {
        cells.add(cell);
    }

    @Override
    public T getCell(int colNum) {

        if (cells.size() <= colNum) {
            String messageTemplate = "Cant retrieve cell number '%s' from table - only has %s cells";
            int actualRowNum = cells.size();
            String message = String.format(messageTemplate, colNum, actualRowNum);
            throw new DocGenException(message);
        }

        return cells.get(colNum);
    }
}
