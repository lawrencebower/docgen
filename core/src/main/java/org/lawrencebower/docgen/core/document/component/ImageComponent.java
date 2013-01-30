package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.springframework.core.io.Resource;

public class ImageComponent extends AbstractDocComponent {

    private Resource imageFileLocation;
    private int width;
    private int height;

    public ImageComponent(Resource imageFileLocation) {
        this.imageFileLocation = imageFileLocation;
    }

    public ImageComponent(HorizontalAlignment alignment, Resource imageFileLocation) {
        super(alignment);
        this.imageFileLocation = imageFileLocation;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.IMAGE;
    }

    public Resource getImageFileLocation() {
        return imageFileLocation;
    }

    public void setSize(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public boolean hasSize() {
        return (width != 0) && (height != 0);
    }
}
