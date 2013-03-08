package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextTextComponent extends AbstractITextComponent<Phrase, TextComponent> {

    @Autowired
    private PDFGenUtils pdfUtils;

    private ITextTextComponent() {
    }

    public String getStringValue() {
        return component.getTextString();
    }

    @Override
    public Phrase createITextComponent() {

        TextBlock textBlock = component.getText();
        Phrase phrase = pdfUtils.mapTextBlock(textBlock);

        return phrase;
    }

}
