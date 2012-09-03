package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

import java.awt.*;

public class TableCell {

    private DocComponent component;

    private DocAlignment verticalAlignment = DocAlignment.TOP;//default
    private DocAlignment horizontalAlignment = DocAlignment.LEFT;//default

    private Color backgroundColor;
    private float padding = 3;//default

    public TableCell(DocComponent component) {
        this.component = component;
    }

    public TableCell(String text) {
        TextBlock textBlock = new TextBlock(text);
        this.component = new TextComponent(textBlock);
    }

    public void setComponent(DocComponent component) {
        this.component = component;
    }

    public DocComponent getComponent() {
        return component;
    }

    public void setText(String text) {
        TextBlock textBlock = new TextBlock(text);
        this.component = new TextComponent(textBlock);
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
