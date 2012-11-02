package org.lawrencebower.docgen.core.generator.custom.component;

import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.AbstractRenderableComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;

public abstract class CustomComponent<T2 extends ITextComponent>
        extends AbstractRenderableComponent<CustomComponentRendererInfo, T2> {

    @Override
    public abstract void createAndRenderComponent(CustomComponentRendererInfo rendererInfo);
}
