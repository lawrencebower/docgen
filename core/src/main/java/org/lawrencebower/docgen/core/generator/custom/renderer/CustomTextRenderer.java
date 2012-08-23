package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTextRenderer implements DocComponentRenderer<TextComponent, CustomComponentRendererInfo> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void renderComponent(TextComponent component, CustomComponentRendererInfo rendererInfo) {
        drawTextBox(rendererInfo, component);
    }

    private void drawTextBox(CustomComponentRendererInfo renderInfo, TextComponent component) {

        TextBlock textBlock = component.getText();

        DocPosition position = component.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        drawBox(renderInfo,
                textBlock,
                boxAlignment);
    }

    private void drawBox(CustomComponentRendererInfo renderInfo,
                         TextBlock textBlock,
                         int boxAlignment) {

        Phrase phrase = pdfUtils.mapTextBlock(textBlock);
        Paragraph paragraph = new Paragraph(phrase);
        paragraph.setAlignment(boxAlignment);
        renderInfo.addToDocument(paragraph);
    }

}
