package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.DocComponentRendererInfo;
import org.lawrencebower.docgen.core.document.RenderableComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;

public abstract class AbstractRenderableComponent
        <T extends DocComponentRendererInfo, T2 extends ITextComponent>
        implements RenderableComponent<T> {

    protected T2 component;

    public abstract void setComponent(T2 component);

    @Override
    public String getName() {
        return component.getName();
    }
}
