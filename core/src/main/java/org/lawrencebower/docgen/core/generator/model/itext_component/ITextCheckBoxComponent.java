package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextCheckBoxComponent extends AbstractITextComponent<Phrase, CheckBoxComponent> {

    @Autowired
    private PDFGenUtils pdfUtils;

    private int FONT_SIZE = 10;//todo maybe work out font size based on box area
    private static final String SELECTED_STRING = "X";
    private static final String UNSELECTED_STRING = "";

    @Override
    public Phrase createITextComponent() {

        boolean selected = component.isSelected();

        String selectedText = getTextFromSelected(selected);

        FontInfo fontInfo = new FontInfo(FontInfo.DEFAULT_FONT,
                                         FONT_SIZE,
                                         FontInfo.DEFAULT_FONT_STYLE);

        TextBlock textBlock = new TextBlock(selectedText, fontInfo);

        return pdfUtils.mapTextBlock(textBlock);
    }

    private String getTextFromSelected(boolean selected) {
        if(selected){
            return SELECTED_STRING;
        }
        return UNSELECTED_STRING;
    }

}
