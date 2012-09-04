package org.lawrencebower.docgen.core.generator.model;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.DocComponent;

public interface DocComponentRenderer<
        T extends DocComponent,
        T2 extends DocComponentRendererInfo,
        T3 extends Element> {

    public void createAndRenderComponent(T component, T2 rendererInfo);

    public T3 createComponent(T component);
}
