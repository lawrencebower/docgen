package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public class TextComponent extends DocComponent {

    private String value;

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

    public void setValue(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

}
