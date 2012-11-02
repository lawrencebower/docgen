package org.lawrencebower.docgen.core.document;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractDocumentInfo<T extends RenderableComponent> implements DocumentInfo<T> {

    protected String name;
    private List<T> components = new ArrayList<>();

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addComponent(T component) {
        this.components.add(component);
    }

    @Override
    public void setComponents(List<T> components) {
        this.components = components;
    }

    @Override
    public List<T> getComponents() {
        return components;
    }

}
