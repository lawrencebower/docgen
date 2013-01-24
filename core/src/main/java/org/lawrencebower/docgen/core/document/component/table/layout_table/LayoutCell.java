package org.lawrencebower.docgen.core.document.component.table.layout_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableCell;

public class LayoutCell extends AbstractTableCell {

    public LayoutCell() {
    }

    public LayoutCell(DocComponent component) {
        this.component = component;
    }

    public LayoutCell(String text) {
        TableTextComponent textComponent = new TableTextComponent(text);
        setComponent(textComponent);
    }
}
