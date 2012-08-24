package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.document.component.text.TextFragment;

import java.awt.*;

public class TableCell {

    private TextBlock text;
    DocAlignment verticalAlignment = DocAlignment.TOP;//default
    DocAlignment horizontalAlignment = DocAlignment.LEFT;//default
    private Color backgroundColor;
    private float padding = 3;//default

    public TableCell(String text) {
        this.text = new TextBlock(text);
    }

    public TableCell(TextBlock text) {
        this.text = text;
    }

    public void setText(String text) {
        this.text = new TextBlock(text);
    }

    public void setText(TextBlock text) {
        this.text = text;
    }

    public TextBlock getText() {
        return text;
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

    public String getTextString() {
        StringBuilder builder = new StringBuilder();
        for (TextFragment textFragment : text.getFragments()) {
            builder.append(textFragment.getText());
        }

        return builder.toString();
    }
}
