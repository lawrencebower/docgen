package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public class CheckBoxComponent extends DocComponent {

    private boolean selected;

    public CheckBoxComponent(boolean selected) {
        this.selected = selected;
    }

    public CheckBoxComponent(boolean selected,
                             HorizontalAlignment alignment) {
        super(alignment);
        this.selected = selected;
    }

    public CheckBoxComponent(HorizontalAlignment alignment) {
        super(alignment);
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
