package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

public class TableTextComponent extends TextComponent {
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
