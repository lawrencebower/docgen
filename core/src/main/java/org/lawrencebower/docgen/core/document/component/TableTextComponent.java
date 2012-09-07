package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;

public class TableTextComponent extends TextComponent {
    public TableTextComponent(TextBlock text) {
        super(text);
    }

    public TableTextComponent(String text) {
        super(text);
    }

    public TableTextComponent(DocPosition position, String text) {
        super(position, text);
    }

    public TableTextComponent(DocPosition position) {
        super(position);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TABLE_TEXT;
    }
}
