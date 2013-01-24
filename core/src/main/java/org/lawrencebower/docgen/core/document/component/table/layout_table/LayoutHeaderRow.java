package org.lawrencebower.docgen.core.document.component.table.layout_table;

import org.lawrencebower.docgen.core.document.component.table.AbstractTableHeaderRow;

import java.util.List;

public class LayoutHeaderRow extends AbstractTableHeaderRow<LayoutHeaderCell> {

    public LayoutHeaderRow() {
        renderHeader = false;//default for layout
    }

    public LayoutHeaderRow(List<LayoutHeaderCell> cells) {
        super(cells);
        renderHeader = false;//default for layout
    }

}
