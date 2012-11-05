package org.lawrencebower.docgen.core.generator.custom.component;

import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomNewLineRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextNewLineComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomNewLineComponent extends CustomComponent<ITextNewLineComponent> {

    @Autowired
    private CustomNewLineRenderer renderer;

    private CustomNewLineComponent() {
        //enforce spring creation
    }

    public void setComponent(ITextNewLineComponent component) {
        this.component = component;
    }

    @Override
    public void createAndRenderComponent(CustomComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
