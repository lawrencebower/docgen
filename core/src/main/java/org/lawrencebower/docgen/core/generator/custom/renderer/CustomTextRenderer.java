package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextTextComponent;

public class CustomTextRenderer
        implements DocComponentRenderer<ITextTextComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextTextComponent component, CustomComponentRendererInfo rendererInfo) {
        Paragraph paragraph = processPhrase(component);
        renderComponent(rendererInfo, paragraph);
    }

    private Paragraph processPhrase(ITextTextComponent component) {

        Phrase iTextPhrase = component.createITextComponent();

        Paragraph paragraph = new Paragraph(iTextPhrase);

        HorizontalAlignment alignment = component.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);
        paragraph.setAlignment(boxAlignment);

        return paragraph;
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element) {
        renderInfo.addToDocument(element);
    }

}
