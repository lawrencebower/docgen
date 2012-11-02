package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.draw.LineSeparator;
import org.lawrencebower.docgen.core.document.component.LineComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextLineComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class CustomLineRenderer
        implements DocComponentRenderer<ITextLineComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextLineComponent component, CustomComponentRendererInfo rendererInfo) {
        renderComponent(rendererInfo, component.createITextComponent());
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element){
        renderInfo.addToDocument(element);
    }
}
