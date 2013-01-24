package org.lawrencebower.docgen.core.document.component.table.layout_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableCell;
import org.lawrencebower.docgen.core.document.component.table.HeaderCell;

public class LayoutHeaderCell extends AbstractTableCell implements HeaderCell {

    private Integer columnWidth;

    public LayoutHeaderCell() {
    }

    public LayoutHeaderCell(DocComponent component) {
        setComponent(component);
    }

    public LayoutHeaderCell(String text) {
        TableTextComponent textComponent = new TableTextComponent(text);
        setComponent(textComponent);
    }

    public LayoutHeaderCell(Integer columnWidth) {
        this.columnWidth = columnWidth;
    }

    public Integer getColumnWidth() {
        return columnWidth;
    }

    public void setColumnWidth(Integer width) {
        columnWidth = width;
    }

    @Override
    public boolean hasColumnWidth() {
        return columnWidth != null;
    }
}
