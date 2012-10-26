package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayCheckBoxRenderer
        extends AbstractOverlayRenderer
        implements DocComponentRenderer<CheckBoxComponent, OverlayComponentRendererInfo, Phrase> {

    private int FONT_SIZE = 10;//todo maybe work out font size based on box area

    @Override
    public void createAndRenderComponent(CheckBoxComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        Phrase phrase = createComponent(component);
        drawCheckBox(rendererInfo.getCanvas(), phrase);
    }

    @Override
    public Phrase createComponent(CheckBoxComponent component) {

        boolean selected = component.getSelected();

        String selectedText = getTextFromSelected(selected);

        FontInfo fontInfo = new FontInfo(FontInfo.DEFAULT_FONT,
                                         FONT_SIZE,
                                         FontInfo.DEFAULT_FONT_STYLE);

        TextBlock textBlock = new TextBlock(selectedText, fontInfo);

        return pdfUtils.mapTextBlock(textBlock);
    }

    private void drawCheckBox(PdfContentByte canvas, Phrase phrase) {

        HorizontalAlignment alignment = docComponent.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);

        DocCoordinates boxCoordinates = docComponent.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        drawBox(canvas,
                phrase,
                boxAlignment,
                boxCoordinates);
    }

    private void drawBox(PdfContentByte canvas,
                         Phrase phrase,
                         int boxAlignment,
                         DocCoordinates boxCoordinates) {

        ColumnText column = createTextColumn(canvas,
                                             boxAlignment,
                                             boxCoordinates,
                                             phrase);

        /**
         * for checkbox - the leading is overridden to be the font size
         */
        column.setLeading(FONT_SIZE);

        drawColumn(column);
    }

    private String getTextFromSelected(boolean selected) {
        if(selected){
            return "X";
        }
        return "";
    }

}
