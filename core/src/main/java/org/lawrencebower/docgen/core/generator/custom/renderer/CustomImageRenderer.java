package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextImageComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

import java.io.IOException;

public class CustomImageRenderer
        implements DocComponentRenderer<ITextImageComponent, CustomComponentRendererInfo> {

    @Override
    public void createAndRenderComponent(ITextImageComponent component, CustomComponentRendererInfo rendererInfo) {
        Image iTextImage = component.createITextComponent();
        processImage(component, iTextImage);
        renderComponent(rendererInfo, iTextImage);
    }

    private void processImage(ITextImageComponent component, Image iTextImage) {

        HorizontalAlignment alignment = component.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);

        iTextImage.setAlignment(boxAlignment);

        scaleImage(component, iTextImage);
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element) {
        renderInfo.addToDocument(element);
    }

    private void scaleImage(ITextImageComponent component, Image iTextImage) {
        if (component.hasSize()) {
            iTextImage.scalePercent(component.getWidth(), component.getHeight());
        }
    }

}
