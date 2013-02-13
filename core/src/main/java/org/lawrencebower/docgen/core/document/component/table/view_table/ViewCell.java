package org.lawrencebower.docgen.core.document.component.table.view_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableCell;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

public class ViewCell extends AbstractTableCell {

    public ViewCell() {
        this("");//default
    }

    public ViewCell(DocComponent component) {
        this.component = component;
    }

    public ViewCell(String text) {
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

    public boolean isTextArea(){//todo this will go when fully nested TableViewComponent is implemented
        String textString = ((TableTextComponent) component).getTextString();
        return textString.length() > 20;
    }
}
