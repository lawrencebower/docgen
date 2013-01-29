package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public abstract class AbstractTableBuilder<T extends TableComponent> {

    protected T table;

    public void setTablePadding(int padding) {
        table.setTablePadding(padding);
    }

    public void setWidthPercentage(int width) {
        table.setWidthPercentage(width);
    }

    public void setRenderBorder(boolean renderBorder) {
        table.setRenderBorder(renderBorder);
    }

    public void setRenderHeader(boolean renderHeader) {
        table.setRenderHeader(renderHeader);
    }

    public void setCoordinates(DocCoordinates coordinates) {
        table.setCoordinates(coordinates);
    }

    public void setAlignment(HorizontalAlignment alignment){
        table.setAlignment(alignment);
    }
}
