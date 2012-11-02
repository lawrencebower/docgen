package org.lawrencebower.docgen.core.generator.custom.component;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomTableTextRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTableTextComponent;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTableTextComponent extends CustomComponent<ITextTableTextComponent> {

    @Autowired
    private CustomTableTextRenderer renderer;


    public void setComponent(ITextTableTextComponent component) {
        this.component = component;
    }

    @Override
    public void createAndRenderComponent(CustomComponentRendererInfo rendererInfo){
        renderer.createAndRenderComponent(component, rendererInfo);
    }

}
