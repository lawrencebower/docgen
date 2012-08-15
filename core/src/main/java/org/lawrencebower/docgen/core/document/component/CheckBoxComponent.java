package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public class CheckBoxComponent extends DocComponent {

    private boolean selected;

    public CheckBoxComponent(String name,
                             boolean selected) {
        super(name);
        this.selected = selected;
    }

    public CheckBoxComponent(String name,
                             boolean selected,
                             DocPosition position) {
        super(name, position);
        this.selected = selected;
    }

    public CheckBoxComponent(String name) {
        super(name);
    }

    public CheckBoxComponent(String name, DocPosition position) {
        super(name, position);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TEXT;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    public boolean getSelected() {
        return selected;
    }

}
