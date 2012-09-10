package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocPosition;

public class LineComponent extends DocComponent {

    private int widthPercentage;

    public LineComponent(int widthPercentage) {
        super();
        this.widthPercentage = widthPercentage;
    }

    public LineComponent(DocPosition position, int widthPercentage) {
        super(position);
        this.widthPercentage = widthPercentage;
    }

    public int getWidthPercentage() {
        return widthPercentage;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.LINE;
    }

}
