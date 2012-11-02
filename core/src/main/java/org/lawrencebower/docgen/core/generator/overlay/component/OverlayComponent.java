package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.generator.model.AbstractRenderableComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class OverlayComponent<T2 extends ITextComponent>
        extends AbstractRenderableComponent<OverlayComponentRendererInfo, T2> {

    @Autowired
    PDFGenUtils pdfUtils;

    @Override
    public abstract void createAndRenderComponent(OverlayComponentRendererInfo rendererInfo);

    public DocCoordinates getCoordinates(){
        return this.component.getCoordinates();
    }

    protected void checkDocCoordinatesSet() {
        pdfUtils.checkCoordinatesPresent(component);
    }

}
