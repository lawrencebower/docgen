package org.lawrencebower.docgen.core.generator.custom.component;

import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomTextRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTextComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTextComponent extends CustomComponent<ITextTextComponent> {

    @Autowired
    private CustomTextRenderer renderer;

    private CustomTextComponent() {
        //enforce spring creation
    }

    public void setComponent(ITextTextComponent component) {
        this.component = component;
    }

    @Override
    public void createAndRenderComponent(CustomComponentRendererInfo rendererInfo) {
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
