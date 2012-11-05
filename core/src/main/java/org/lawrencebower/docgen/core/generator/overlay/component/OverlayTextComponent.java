package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTextComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.overlay.renderer.OverlayTextRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayTextComponent extends OverlayComponent<ITextTextComponent> {

    @Autowired
    private OverlayTextRenderer renderer;

    private OverlayTextComponent() {
        //enforce spring creation
    }

    public void setComponent(ITextTextComponent component) {
        this.component = component;
        checkDocCoordinatesSet();
    }

    @Override
    public void createAndRenderComponent(OverlayComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
