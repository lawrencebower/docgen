package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public abstract class DocComponent {

    private String name;
    private HorizontalAlignment alignment;
    private DocCoordinates coordinates;
    protected boolean renderBorder;

    protected DocComponent() {
        this.alignment = HorizontalAlignment.LEFT;
    }

    protected DocComponent(HorizontalAlignment alignment) {
        this.alignment = alignment;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public HorizontalAlignment getAlignment() {
        return alignment;
    }

    public void setAlignment(HorizontalAlignment alignment) {
        this.alignment = alignment;
    }

    public DocCoordinates getCoordinates() {
        return coordinates;
    }

    public void setCoordinates(DocCoordinates coordinates) {
        this.coordinates = coordinates;
    }

    public abstract DocComponentType getComponentType();

    public void setRenderBorder(boolean renderBorder) {
        this.renderBorder = renderBorder;
    }

    public boolean isRenderBorder() {
        return renderBorder;
    }
}
