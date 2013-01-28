package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.AbstractDocComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractTableComponent<
        T extends TableRow,
        T2 extends TableCell,
        T3 extends HeaderCell,
        T4 extends TableHeaderRow> extends AbstractDocComponent implements TableComponent<T, T2, T3, T4> {

    protected T4 headerRow;
    protected List<T> rows = new ArrayList<>();
    private float widthPercentage;
    private int tablePadding = 3;//default

    public AbstractTableComponent(HorizontalAlignment alignment) {
        super(alignment);
    }

    @Override
    public int getColumnCount() {
        return headerRow.getColumnCount();
    }

    public void setHeaderRow(T4 row) {
        headerRow = row;
    }

    @Override
    public T4 getHeaderRow() {
        return headerRow;
    }

    @Override
    public void addRow(T row) {
        rows.add(row);
    }

    @Override
    public List<T> getRows() {
        return rows;
    }

    @Override
    public void clearRows() {
        rows.clear();
    }

    @Override
    public void setWidthPercentage(float widthPercentage) {
        this.widthPercentage = widthPercentage;
    }

    @Override
    public float getWithPercentage() {
        return widthPercentage;
    }

    @Override
    public int[] getColumnWidths() {
        return headerRow.getColumnWidths();
    }

    /**
     * will be over ridden by padding set on any given cell
     */
    @Override
    public void setTablePadding(int tablePadding) {
        this.tablePadding = tablePadding;
    }

    @Override
    public int getTablePadding() {
        return tablePadding;
    }

    @Override
    public T getRow(int rowNum) {

        if (rows.size() <= rowNum) {
            String messageTemplate = "Cant retrieve row number '%s' from table '%s' - only has %s rows";
            String componentName = getName();
            int actualRowNum = rows.size();
            String message = String.format(messageTemplate, rowNum, componentName, actualRowNum);
            throw new DocGenException(message);
        }

        return rows.get(rowNum);
    }

    @Override
    public void setRenderHeader(boolean renderHeader) {
        if(headerRow != null){
            headerRow.setRenderHeader(renderHeader);
        }
    }
}
