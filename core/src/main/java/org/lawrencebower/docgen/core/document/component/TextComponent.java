package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

public class TextComponent extends DocComponent {

    private TextBlock text;

    public TextComponent(String name, TextBlock text) {
        super(name);
        this.text = text;
    }

    public TextComponent(String name,
                         String text) {
        super(name);
        this.text = new TextBlock(text);
    }

    public TextComponent(String name,
                         DocPosition position,
                         String text) {
        super(name, position);
        this.text = new TextBlock(text);
    }

    public TextComponent(String name) {
        super(name);
    }

    public TextComponent(String name, DocPosition position) {
        super(name, position);
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

}
