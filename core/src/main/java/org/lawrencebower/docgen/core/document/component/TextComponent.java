package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
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

    public TextComponent(HorizontalAlignment alignment,
                         String text) {
        super(alignment);
        this.text = new TextBlock(text);
    }

    public TextComponent(HorizontalAlignment alignment) {
        super(alignment);
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
            String fragment = textFragment.getText();
            builder.append(fragment);
        }

        return builder.toString();
    }

}
