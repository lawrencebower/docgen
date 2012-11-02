package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Image;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.IOException;

public class ITextImageComponent extends AbstractITextComponent<Image, ImageComponent> {

    @Override
    public Image createITextComponent() {
        try {
            String imageFileLocation = component.getImageFileLocation();
            Image iTextImage = Image.getInstance(imageFileLocation);

            return iTextImage;
        } catch (BadElementException | IOException e) {
            throw new DocGenException(e);
        }
    }

    public boolean hasSize() {
        return component.hasSize();
    }

    public float getWidth() {
        return component.getWidth();
    }

    public float getHeight() {
        return component.getHeight();
    }
}
