package org.lawrencebower.docgen.core.generator.custom.component;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.LineComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomLineRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextLineComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomLineComponent extends CustomComponent<ITextLineComponent> {

    @Autowired
    private CustomLineRenderer renderer;

    public void setComponent(ITextLineComponent component) {
        this.component = component;
    }

    @Override
    public void createAndRenderComponent(CustomComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
