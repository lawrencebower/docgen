package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableBuilder;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutRow;

public class ViewTableBuilder extends AbstractTableBuilder<ViewTableComponent> {

    public ViewTableBuilder(String tableName) {
        this.table = new ViewTableComponent(tableName);
    }

    public ViewTableComponent getTable() {
        return table;
    }

    public void setHeaderRow(ViewHeaderRow headerRow) {
        table.setHeaderRow(headerRow);
    }

    public void addRowWithComponents(String rowName, DocComponent... components) {
        ViewRow row = new ViewRow(rowName);
        for (DocComponent component : components) {
            ViewCell cell = makeCell(component);
            row.addCell(cell);
        }
       table.addRow(row);
    }

    public ViewCell makeCell(DocComponent component) {
        return new ViewCell(component);
    }

    private LayoutRow makeRow() {
        return new LayoutRow();
    }

}
