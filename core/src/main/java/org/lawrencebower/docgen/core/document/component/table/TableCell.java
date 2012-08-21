package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.position.DocAlignment;

import java.awt.*;

public class TableCell {

    private String value;
    DocAlignment verticalAlignment = DocAlignment.TOP;//default
    DocAlignment horizontalAlignment = DocAlignment.LEFT;//default
    private Color backgroundColor;
    private float padding = 3;//default

    public TableCell(String value) {
        this.value = value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public DocAlignment getVerticalAlignment() {
        return verticalAlignment;
    }

    public void setVerticalAlignment(DocAlignment verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }

    public DocAlignment getHorizontalAlignment() {
        return horizontalAlignment;
    }

    public void setHorizontalAlignment(DocAlignment horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }

    public void setBackgroundColor(Color backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    public Color getBackgroundColor() {
        return backgroundColor;
    }

    public boolean hasBackgroundColor(){
        return backgroundColor != null;
    }

    public void setPadding(float padding) {
        this.padding = padding;
    }

    public float getPadding() {
        return padding;
    }
}
