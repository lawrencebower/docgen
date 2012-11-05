package org.lawrencebower.docgen.core.generator.custom.component;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomTableRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTableComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTableComponent extends CustomComponent<ITextTableComponent> {

    @Autowired
    private CustomTableRenderer renderer;

    private CustomTableComponent() {
        //enforce spring creation
    }

    public void setComponent(ITextTableComponent component) {
        this.component = component;
    }

    @Override
    public void createAndRenderComponent(CustomComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
