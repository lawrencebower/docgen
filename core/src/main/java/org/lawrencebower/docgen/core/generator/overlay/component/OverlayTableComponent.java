package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTableComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.overlay.renderer.OverlayTableRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayTableComponent extends OverlayComponent<ITextTableComponent> {

    @Autowired
    private OverlayTableRenderer renderer;

    private OverlayTableComponent() {
        //enforce spring creation
    }

    public void setComponent(ITextTableComponent component) {
        this.component = component;
        checkDocCoordinatesSet();
    }

    @Override
    public void createAndRenderComponent(OverlayComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
