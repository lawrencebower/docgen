package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.generator.model.itext_component.ITextCheckBoxComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.overlay.renderer.OverlayCheckBoxRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayCheckBoxComponent extends OverlayComponent<ITextCheckBoxComponent> {

    @Autowired
    private OverlayCheckBoxRenderer renderer;

    private OverlayCheckBoxComponent() {//enforce spring creation
    }

    public void setComponent(ITextCheckBoxComponent component) {
        this.component = component;
        checkDocCoordinatesSet();
    }

    @Override
    public void createAndRenderComponent(OverlayComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
