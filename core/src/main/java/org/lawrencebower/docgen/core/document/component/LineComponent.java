package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public class LineComponent extends DocComponent {

    private int widthPercentage;

    public LineComponent(int widthPercentage) {
        super();
        this.widthPercentage = widthPercentage;
    }

    public LineComponent(HorizontalAlignment alignment,
                         int widthPercentage) {
        super(alignment);
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
