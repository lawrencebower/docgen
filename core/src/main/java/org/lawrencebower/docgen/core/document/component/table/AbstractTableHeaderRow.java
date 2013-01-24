package org.lawrencebower.docgen.core.document.component.table;

import org.apache.commons.lang.ArrayUtils;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractTableHeaderRow<T extends HeaderCell>
        extends AbstractTableRow<T> implements TableHeaderRow<T> {

    protected boolean renderHeader = true;

    protected AbstractTableHeaderRow() {
    }

    protected AbstractTableHeaderRow(List<T> cells) {
        super(cells);
    }

    @Override
    public boolean isRenderHeader() {
        return renderHeader;
    }

    @Override
    public void setRenderHeader(boolean renderHeader) {
        this.renderHeader = renderHeader;
    }

    @Override
    public void addCell(T cell) {
        super.addCell(cell);
        clearAllColumnWidthsIfNotSet(cell);
    }

    protected void clearAllColumnWidthsIfNotSet(T cell) {
        if (cell.getColumnWidth() == null) {
            for (T t : cells) {
                t.setColumnWidth(null);
            }
        }
    }

    public int[] getColumnWidths() {

        List<Integer> integerSizes = new ArrayList<>();

        for (T headerCell : getCells()) {
            if (headerCell.hasColumnWidth()) {
                Integer columnWidth = headerCell.getColumnWidth();
                integerSizes.add(columnWidth);
            }
        }

        Integer[] integers = integerSizes.toArray(new Integer[integerSizes.size()]);

        return ArrayUtils.toPrimitive(integers);
    }

    public void setColumnWidths(Integer... widths) {

        int columnCount = getColumnCount();

        if (widths.length != columnCount) {
            String message = "Specified number of column widths '%s' does not match the number of columns - '%s'";
            String formattedMessage = String.format(message, widths.length, columnCount);
            throw new DocGenException(formattedMessage);
        }

        for (int i = 0; i < columnCount; i++) {
            T headerCell = cells.get(i);
            headerCell.setColumnWidth(widths[i]);
        }

    }
}
