package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.Paragraph;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextNewLineComponent;

public class CustomNewLineRenderer
        implements DocComponentRenderer<ITextNewLineComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextNewLineComponent component, CustomComponentRendererInfo rendererInfo) {
        Paragraph iTextNewLine = component.createITextComponent();
        renderComponent(rendererInfo, iTextNewLine);
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element){
        renderInfo.addToDocument(element);
    }
}
