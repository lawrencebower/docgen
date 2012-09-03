package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.Paragraph;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;

public class CustomNewLineRenderer implements CustomDocComponentRenderer<NewLineComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(NewLineComponent component, CustomComponentRendererInfo rendererInfo) {
        Element element = createComponent(component);
        renderComponent(rendererInfo, element);
    }

    @Override
    public Element createComponent(NewLineComponent component) {
        return new Paragraph("\n");
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element){
        renderInfo.addToDocument(element);
    }
}
