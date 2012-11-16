package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TableComponent extends DocComponent {

    private TableHeaderRow headerRow = new TableHeaderRow();
    private List<TableRow> rows = new ArrayList<>();
    private float widthPercentage;
    private int tablePadding = 3;//default

    public TableComponent(String name) {
        setName(name);
    }

    public TableComponent(String name, HorizontalAlignment alignment) {
        super(alignment);
        setName(name);
    }

    public int getColumnCount() {
        return headerRow.getColumnCount();
    }

    public void setHeaderRow(TableHeaderRow row) {
        headerRow = row;
    }

    public void setHeaderRow(TableCell... cells) {
        headerRow = new TableHeaderRow();
        headerRow.setCells(Arrays.asList(cells));
    }

    public TableHeaderRow getHeaderRow() {
        return headerRow;
    }

    public void addRow(TableRow row) {
        rows.add(row);
    }

    public List<TableRow> getRows() {
        return rows;
    }

    public void clearRows() {
        rows.clear();
    }

    public List<TableCell> getAllRenderableCells() {
        List<TableCell> allCells = new ArrayList<>();

        if (headerRow.isRenderHeader()) {
            allCells.addAll(headerRow.getCells());
        }

        for (TableRow row : rows) {
            allCells.addAll(row.getCells());
        }

        return allCells;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TABLE;
    }

    public void setWidthPercentage(float widthPercentage) {
        this.widthPercentage = widthPercentage;
    }

    public float getWithPercentage() {
        return widthPercentage;
    }

    public int[] getColumnWidths() {
        return headerRow.getColumnWidths();
    }

    /**
     * will be over ridden by padding set on any given cell
     */
    public void setTablePadding(int tablePadding) {
        this.tablePadding = tablePadding;
    }

    public int getTablePadding() {
        return tablePadding;
    }

    public TableRow getRow(int rowNum) {

        if(rows.size() <=  rowNum){
            String messageTemplate = "Cant retrieve row number '%s' from table '%s' - only has %s rows";
            String componentName = getName();
            int actualRowNum = rows.size();
            String message = String.format(messageTemplate, rowNum, componentName, actualRowNum);
            throw new DocGenException(message);
        }

        return rows.get(rowNum);
    }

}
