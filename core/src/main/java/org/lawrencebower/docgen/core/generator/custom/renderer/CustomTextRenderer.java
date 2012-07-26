package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Paragraph;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.generator.custom.rendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTextRenderer implements DocComponentRenderer<TextComponent, rendererInfo> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void renderComponent(TextComponent component, rendererInfo rendererInfo) {
        drawTextBox(rendererInfo, component);
    }

    private void drawTextBox(rendererInfo renderInfo, TextComponent component) {

        String boxText = component.getValue();

        DocPosition position = component.getPosition();
        int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

        drawBox(renderInfo,
                boxText,
                boxAlignment);
    }

    private void drawBox(rendererInfo renderInfo,
                         String boxText,
                         int boxAlignment) {//todo make use of alignment
        Paragraph paragraph = new Paragraph(boxText);
        renderInfo.addToDocument(paragraph);
    }

}
