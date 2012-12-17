package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.generator.model.AbstractRenderableComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.utils.ITextComponentUtils;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class OverlayComponent<T2 extends ITextComponent>
        extends AbstractRenderableComponent<OverlayComponentRendererInfo, T2> {

    @Autowired
    ITextComponentUtils iTextUtils;

    public void setiTextUtils(ITextComponentUtils iTextUtils) {
        this.iTextUtils = iTextUtils;
    }

    @Override
    public abstract void createAndRenderComponent(OverlayComponentRendererInfo rendererInfo);

    protected void checkDocCoordinatesSet() {
        iTextUtils.checkCoordinatesPresent(component);
    }

}
