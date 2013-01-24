package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public abstract class AbstractDocComponent implements DocComponent {

    private String name;
    private HorizontalAlignment alignment;
    private DocCoordinates coordinates;
    protected boolean renderBorder;

    protected AbstractDocComponent() {
        this.alignment = HorizontalAlignment.LEFT;
    }

    protected AbstractDocComponent(HorizontalAlignment alignment) {
        this.alignment = alignment;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public HorizontalAlignment getAlignment() {
        return alignment;
    }

    @Override
    public void setAlignment(HorizontalAlignment alignment) {
        this.alignment = alignment;
    }

    @Override
    public DocCoordinates getCoordinates() {
        return coordinates;
    }

    @Override
    public void setCoordinates(DocCoordinates coordinates) {
        this.coordinates = coordinates;
    }

    @Override
    public void setRenderBorder(boolean renderBorder) {
        this.renderBorder = renderBorder;
    }

    @Override
    public boolean isRenderBorder() {
        return renderBorder;
    }
}
