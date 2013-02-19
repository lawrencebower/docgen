package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

public class TableTextComponent extends TextComponent {

    public TableTextComponent() {
    }

    public TableTextComponent(TextComponent component) {
        super(component.getText());
        String name = component.getName();
        setName(name);
    }

    public TableTextComponent(TextBlock text) {
        super(text);
    }

    public TableTextComponent(String text) {
        super(text);
    }

    public TableTextComponent(HorizontalAlignment alignment, String text) {
        super(alignment, text);
    }

    public TableTextComponent(HorizontalAlignment alignment) {
        super(alignment);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TABLE_TEXT;
    }
}
