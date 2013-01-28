package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.table.AbstractTableRow;

public class ViewRow extends AbstractTableRow<ViewCell> {

    protected String rowName;

    public ViewRow(String rowName) {
        this.rowName = rowName;
    }

    public boolean hasCellName(String cellName) {

        boolean hasCell = false;

        for (ViewCell cell : cells) {
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
