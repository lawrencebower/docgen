package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public class TextComponent extends DocComponent {

    private String value;
    private boolean renderBorder;

    public TextComponent(String name,
                         String value) {
        super(name);
        this.value = value;
    }

    public TextComponent(String name,
                         DocPosition position,
                         String value) {
        super(name, position);
        this.value = value;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TEXT;
    }

    public String getValue() {
        return value;
    }

    public boolean isRenderBorder() {
        return renderBorder;
    }

    public void setRenderBorder(boolean renderBorder) {
        this.renderBorder = renderBorder;
    }
}
