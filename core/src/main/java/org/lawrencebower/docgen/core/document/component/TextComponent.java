package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.document.component.text.TextFragment;

public class TextComponent extends DocComponent {

    private TextBlock text;

    public TextComponent(TextBlock text) {
        this.text = text;
    }

    public TextComponent(String text) {
        this.text = new TextBlock(text);
    }

    public TextComponent(DocPosition position,
                         String text) {
        super(position);
        this.text = new TextBlock(text);
    }

    public TextComponent(DocPosition position) {
        super(position);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TEXT;
    }

    public void setText(String text) {
        this.text = new TextBlock(text);
    }

    public TextBlock getText() {
        return text;
    }

    public String getTextString() {
        StringBuilder builder = new StringBuilder();
        for (TextFragment textFragment : text.getFragments()) {
            builder.append(textFragment.getText());
        }

        return builder.toString();
    }

}
