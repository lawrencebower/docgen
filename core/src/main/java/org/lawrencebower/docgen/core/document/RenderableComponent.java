package org.lawrencebower.docgen.core.document;

public interface RenderableComponent<T extends DocComponentRendererInfo> {

    public void createAndRenderComponent(T rendererInfo);

    public String getName();
}
