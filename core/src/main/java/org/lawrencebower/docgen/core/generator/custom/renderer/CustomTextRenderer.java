package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTextRenderer implements CustomDocComponentRenderer<TextComponent, CustomComponentRendererInfo> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void createAndRenderComponent(TextComponent component, CustomComponentRendererInfo rendererInfo) {
        Element element = createComponent(component);
        renderComponent(rendererInfo, element);
    }

    @Override
    public Element createComponent(TextComponent component) {

        TextBlock textBlock = component.getText();

        DocPosition position = component.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        Phrase phrase = pdfUtils.mapTextBlock(textBlock);
        Paragraph paragraph = new Paragraph(phrase);
        paragraph.setAlignment(boxAlignment);
        return paragraph;
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element){
        renderInfo.addToDocument(element);
    }

}
