package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.table.AbstractTableRow;

public class ViewTableRow extends AbstractTableRow<ViewTableCell> {

    protected String rowName;

    public ViewTableRow(String rowName) {
        this.rowName = rowName;
    }

    public boolean hasCellName(String cellName) {

        boolean hasCell = false;

        for (ViewTableCell cell : cells) {
            String name = cell.getName();
            if(name.equals(cellName)){
                hasCell = true;
            }
        }

        return hasCell;
    }

    public String getRowName() {
        return rowName;
    }
}
