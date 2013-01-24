package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;

import java.awt.*;

public abstract class AbstractTableCell implements TableCell {

    /**
     * horizontal alignment is sourced from the inner DocComponent
     */
    private VerticalAlignment verticalAlignment = VerticalAlignment.TOP;//default
    private Color backgroundColor;
    private float padding = -1;//negative by default - tableCellPadding is used unless this is positive
    private int rowSpan = 1;//default
    private int colSpan = 1;//default
    protected DocComponent component;

    @Override
    public VerticalAlignment getVerticalAlignment() {
        return verticalAlignment;
    }

    @Override
    public void setVerticalAlignment(VerticalAlignment verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }

    @Override
    public void setBackgroundColor(Color backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    @Override
    public Color getBackgroundColor() {
        return backgroundColor;
    }

    @Override
    public boolean hasBackgroundColor(){
        return backgroundColor != null;
    }

    @Override
    public void setPadding(float padding) {
        this.padding = padding;
    }

    @Override
    public float getPadding() {
        return padding;
    }

    @Override
    public void setRowSpan(int rowSpan) {
        this.rowSpan = rowSpan;
    }

    @Override
    public int getRowSpan() {
        return rowSpan;
    }

    @Override
    public void setColSpan(int colSpan) {
        this.colSpan = colSpan;
    }

    @Override
    public int getColSpan() {
        return colSpan;
    }

    @Override
    public void setComponent(DocComponent component) {
        this.component = component;
    }

    public DocComponent getComponent() {
        return component;
    }
}
