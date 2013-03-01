package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableCell;
import org.lawrencebower.docgen.core.document.component.table.HeaderCell;

public class ViewHeaderCell extends AbstractTableCell implements HeaderCell {

    private String name;
    private String text;
    private Integer columnWidth;
    private boolean copyAlignment;

    public ViewHeaderCell(String name, Integer columnWidth) {
        this.name = name;
        this.text = name;//default to name if not set otherwise
        this.columnWidth = columnWidth;
    }

    public ViewHeaderCell(String name) {
        this.name = name;
        this.text = name;//default to name if not set otherwise
    }

    public void setText(String text) {
        this.text = text;
    }

//    public String getText() {
//        return text;
//    }

    public String getName() {
        return name;
    }

    public Integer getColumnWidth() {
        return columnWidth;
    }

    public void setColumnWidth(Integer width) {
        columnWidth = width;
    }

    @Override
    public DocComponent getComponent() {
        if(component == null){
            component = new TableTextComponent(text);
        }

        return component;
    }

    @Override
    public boolean hasColumnWidth() {
        return columnWidth != null;
    }

    public void setCopyAlignment(boolean copyAlignment) {
        this.copyAlignment = copyAlignment;
    }

    public boolean isCopyAlignment() {
        return copyAlignment;
    }
}
