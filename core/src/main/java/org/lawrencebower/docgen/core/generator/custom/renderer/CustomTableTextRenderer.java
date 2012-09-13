package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Phrase;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class CustomTableTextRenderer
        implements CustomDocComponentRenderer<TextComponent, CustomComponentRendererInfo, Phrase> {

    @Autowired
    private PDFGenUtils pdfUtils;

    /**
     * not implemented for TableText - this component will never render its self, only be added to a parent
     * table, which deals with the rendering
     */
    @Override
    public void createAndRenderComponent(TextComponent component, CustomComponentRendererInfo rendererInfo) {
        throw new NotImplementedException();
    }

    @Override
    public Phrase createComponent(TextComponent component) {

        TextBlock textBlock = component.getText();

        return pdfUtils.mapTextBlock(textBlock);
    }

}
