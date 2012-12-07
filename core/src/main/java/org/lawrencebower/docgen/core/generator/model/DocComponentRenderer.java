package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.DocComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;

public interface DocComponentRenderer<
        T extends ITextComponent,
        T2 extends DocComponentRendererInfo> {

    void createAndRenderComponent(T component, T2 rendererInfo);

}
