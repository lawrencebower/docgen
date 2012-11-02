package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.generator.model.itext_component.ITextImageComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.overlay.renderer.OverlayImageRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayImageComponent extends OverlayComponent<ITextImageComponent> {

    @Autowired
    private OverlayImageRenderer renderer;

    public void setComponent(ITextImageComponent component) {
        this.component = component;
        checkDocCoordinatesSet();
    }

    @Override
    public void createAndRenderComponent(OverlayComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
