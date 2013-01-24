package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableCell;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

public class ViewTableCell extends AbstractTableCell {

    public ViewTableCell() {
        this("");//default
    }

    public ViewTableCell(DocComponent component) {
        this.component = component;
    }

    public ViewTableCell(String text) {
        TextBlock textBlock = new TextBlock(text);
        component = new TableTextComponent(textBlock);
    }

    public void setValue(String text) {
        TextBlock textBlock = new TextBlock(text);
        component = new TableTextComponent(textBlock);
    }

    public void setName(String name) {
        component.setName(name);
    }

    public String getName() {
        return component.getName();
    }

}
