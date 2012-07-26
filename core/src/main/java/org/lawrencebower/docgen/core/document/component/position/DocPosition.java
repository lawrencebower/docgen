package org.lawrencebower.docgen.core.document.component.position;

public class DocPosition {
    private DocAlignment alignment;
    private DocCoordinates coordinates;

    public DocPosition(DocAlignment alignment,
                       DocCoordinates coordinates) {
        this.alignment = alignment;
        this.coordinates = coordinates;
    }

    public DocPosition(DocAlignment alignment) {
        this.alignment = alignment;
    }

    public DocAlignment getAlignment() {
        return alignment;
    }

    public DocCoordinates getCoordinates() {
        return coordinates;
    }
}
