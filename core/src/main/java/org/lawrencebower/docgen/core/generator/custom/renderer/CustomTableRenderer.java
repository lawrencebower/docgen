package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTableComponent;

public class CustomTableRenderer
        implements DocComponentRenderer<ITextTableComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextTableComponent component, CustomComponentRendererInfo rendererInfo) {
        PdfPTable iTextTable = component.createITextComponent();
        renderComponent(rendererInfo, iTextTable);
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element) {
        renderInfo.addToDocument(element);
    }
}
