package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.draw.LineSeparator;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextLineComponent;

public class CustomLineRenderer
        implements DocComponentRenderer<ITextLineComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextLineComponent component, CustomComponentRendererInfo rendererInfo) {
        LineSeparator iTextLine = component.createITextComponent();
        renderComponent(rendererInfo, iTextLine);
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element){
        renderInfo.addToDocument(element);
    }
}
