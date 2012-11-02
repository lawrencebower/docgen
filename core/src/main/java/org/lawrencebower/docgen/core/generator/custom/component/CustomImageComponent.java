package org.lawrencebower.docgen.core.generator.custom.component;

import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomImageRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextImageComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomImageComponent extends CustomComponent<ITextImageComponent> {

    @Autowired
    private CustomImageRenderer renderer;

    public void setComponent(ITextImageComponent component) {
        this.component = component;
    }

    @Override
    public void createAndRenderComponent(CustomComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }
}
