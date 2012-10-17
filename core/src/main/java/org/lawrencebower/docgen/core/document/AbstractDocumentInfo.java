package org.lawrencebower.docgen.core.document;

import org.lawrencebower.docgen.core.document.component.DocComponent;

import java.util.List;

public abstract class AbstractDocumentInfo implements DocumentInfo {

    protected String name;
    private List<DocComponent> components;

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void setComponents(List<DocComponent> components) {
        this.components = components;
    }

    @Override
    public List<DocComponent> getComponents() {
        return components;
    }

}
