package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;

import java.io.IOException;

public class CustomImageRenderer
        implements CustomDocComponentRenderer<ImageComponent, CustomComponentRendererInfo, Image> {

    @Override
    public void createAndRenderComponent(ImageComponent component, CustomComponentRendererInfo rendererInfo) {
        Element element = createComponent(component);
        renderComponent(rendererInfo, element);
    }

    @Override
    public Image createComponent(ImageComponent component) {

        try {
            String imageFileLocation = component.getImageFileLocation();
            Image iTextImage = Image.getInstance(imageFileLocation);

            DocPosition position = component.getPosition();
            int boxAlignment = DocAlignment.mapToITextAlignment(position.getAlignment());

            iTextImage.setAlignment(boxAlignment);

            scaleImage(component, iTextImage);

            return iTextImage;
        } catch (BadElementException | IOException e) {
            throw new DocGenException(e);
        }
    }

    private void scaleImage(ImageComponent component, Image iTextImage) {
        if(component.hasSize()){
            iTextImage.scalePercent(component.getWidth(), component.getHeight());
        }
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element) {
        renderInfo.addToDocument(element);
    }

}
