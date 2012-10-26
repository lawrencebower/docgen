package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.draw.LineSeparator;
import org.lawrencebower.docgen.core.document.component.LineComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;

public class CustomLineRenderer
        implements DocComponentRenderer<LineComponent, CustomComponentRendererInfo, LineSeparator> {

    @Override
    public void createAndRenderComponent(LineComponent component, CustomComponentRendererInfo rendererInfo) {
        Element element = createComponent(component);
        renderComponent(rendererInfo, element);
    }

    @Override
    public LineSeparator createComponent(LineComponent component) {

        HorizontalAlignment alignment = component.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);

        return new LineSeparator(1,
                                 component.getWidthPercentage(),
                                 null,
                                 boxAlignment,
                                 0);
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element){
        renderInfo.addToDocument(element);
    }
}
