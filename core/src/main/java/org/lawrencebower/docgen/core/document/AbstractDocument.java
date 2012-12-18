package org.lawrencebower.docgen.core.document;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractDocument<T extends RenderableComponent> implements Document<T> {

    protected String name;
    private List<T> components = new ArrayList<>();

    @Override
    public void setName(String name) {

        if(StringUtils.isWhitespace(name)){
            throw new DocGenException("Name is not set");
        }

        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addComponent(T component) {
        components.add(component);
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
