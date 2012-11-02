package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextTableTextComponent extends AbstractITextComponent<Phrase, TableTextComponent> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public Phrase createITextComponent() {
        TextBlock textBlock = component.getText();
        return pdfUtils.mapTextBlock(textBlock);
    }
}
