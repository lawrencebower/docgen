package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public abstract class DocComponent {

    private String name;
    private DocPosition position;
    protected boolean renderBorder;

    protected DocComponent() {
        this.position = new DocPosition(DocAlignment.LEFT);
    }

    protected DocComponent(DocPosition position) {
        this.position = position;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setPosition(DocPosition position) {
        this.position = position;
    }

    public DocPosition getPosition() {
        return position;
    }

    public abstract DocComponentType getComponentType();

    public void setRenderBorder(boolean renderBorder) {
        this.renderBorder = renderBorder;
    }

    public boolean isRenderBorder() {
        return renderBorder;
    }
}
