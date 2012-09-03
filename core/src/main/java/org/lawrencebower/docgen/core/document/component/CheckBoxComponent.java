package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public class CheckBoxComponent extends DocComponent {

    private boolean selected;

    public CheckBoxComponent(boolean selected) {
        this.selected = selected;
    }

    public CheckBoxComponent(boolean selected,
                             DocPosition position) {
        super(position);
        this.selected = selected;
    }

    public CheckBoxComponent() {
    }

    public CheckBoxComponent(DocPosition position) {
        super(position);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.CHECKBOX;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    public boolean getSelected() {
        return selected;
    }

}
