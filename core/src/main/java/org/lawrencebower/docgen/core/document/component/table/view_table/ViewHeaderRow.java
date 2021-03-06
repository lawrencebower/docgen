package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.table.AbstractTableHeaderRow;

import java.util.List;

public class ViewHeaderRow extends AbstractTableHeaderRow<ViewHeaderCell> {

    public ViewHeaderRow() {
    }

    public ViewHeaderRow(List<ViewHeaderCell> cells) {
        super(cells);
    }

    public boolean hasColumnName(String columnName) {

        boolean hasColumn = false;

        for (ViewHeaderCell cell : cells) {
            String cellName = cell.getName();
            if(cellName.equals(columnName)){
                hasColumn = true;
            }
        }

        return hasColumn;
    }
}
