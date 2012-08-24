package org.lawrencebower.docgen.core.document.component.table;

import org.apache.commons.lang.ArrayUtils;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TableHeaderRow extends TableRow {

    private List<Integer> columnWidths = new ArrayList<>();

    @Override
    public void addCell(TableCell cell) {
        super.addCell(cell);
        columnWidths.clear();//if you add a cell without a width - all other widths cleared
    }

    public void addCell(TableCell cell, int relativeWidth){
        super.addCell(cell);
        columnWidths.add(relativeWidth);
    }

    public int[] getColumnWidths(){
        Integer[] integerSizes = columnWidths.toArray(new Integer[columnWidths.size()]);
        return ArrayUtils.toPrimitive(integerSizes);
    }

    public void setColumnWidths(Integer... widths) {

        int columnCount = getColumnCount();

        if(widths.length != columnCount){
            String message = "Specified number of column widths '%s' does not match the number of columns - '%s'";
            throw new DocGenException(String.format(message, widths.length, columnCount));
        }

        columnWidths = new ArrayList<>(Arrays.asList(widths));
    }
}