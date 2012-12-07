package org.lawrencebower.docgen.core.document;

public interface RenderableComponent<T extends DocComponentRendererInfo> {

    void createAndRenderComponent(T rendererInfo);

}
