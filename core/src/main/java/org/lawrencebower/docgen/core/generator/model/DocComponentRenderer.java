package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.component.DocComponent;

public interface DocComponentRenderer<T extends DocComponent, T2 extends DocComponentRendererInfo> {

    public void createAndRenderComponent(T component, T2 rendererInfo);

}
