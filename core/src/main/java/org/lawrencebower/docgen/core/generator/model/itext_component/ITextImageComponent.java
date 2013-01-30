package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Image;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.DocGenFileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;

import java.io.IOException;
import java.io.InputStream;

public class ITextImageComponent extends AbstractITextComponent<Image, ImageComponent> {

    @Autowired
    private DocGenFileUtils fileUtils;

    @Override
    public Image createITextComponent() {
        try {

            Resource imageResource = component.getImageFileLocation();

            InputStream inputStream = imageResource.getInputStream();

            byte[] bytes = fileUtils.getAllBytesFromStream(inputStream);

            return Image.getInstance(bytes);

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
