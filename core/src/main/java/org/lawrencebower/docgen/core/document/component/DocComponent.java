package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public abstract class DocComponent {

    private String name;
    private DocPosition position;

    protected DocComponent(String name) {
        this.name = name;
        this.position = new DocPosition(DocAlignment.LEFT);
    }

    protected DocComponent(String name,
                           DocPosition position) {
        this.name = name;
        this.position = position;
    }

    public String getName() {
        return name;
    }

    public DocPosition getPosition() {
        return position;
    }

    public abstract DocComponentType getComponentType();

}
