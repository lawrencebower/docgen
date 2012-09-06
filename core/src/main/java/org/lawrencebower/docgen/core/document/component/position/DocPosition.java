package org.lawrencebower.docgen.core.document.component.position;

public class DocPosition {

    private HorizontalAlignment horizontalAlignment = HorizontalAlignment.LEFT;//default

    private DocCoordinates coordinates;

    public DocPosition(HorizontalAlignment horizontalAlignment,
                       DocCoordinates coordinates) {
        this.horizontalAlignment = horizontalAlignment;
        this.coordinates = coordinates;
    }

    public DocPosition(DocCoordinates coordinates) {
        this.coordinates = coordinates;
    }

    public DocPosition(HorizontalAlignment horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }

    public void setHorizontalAlignment(HorizontalAlignment horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }

    public HorizontalAlignment getHorizontalAlignment() {
        return horizontalAlignment;
    }

    public DocCoordinates getCoordinates() {
        return coordinates;
    }
}
