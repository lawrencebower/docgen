package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;

import java.awt.*;

public interface TableCell {

    VerticalAlignment getVerticalAlignment();

    void setVerticalAlignment(VerticalAlignment verticalAlignment);

    void setBackgroundColor(Color backgroundColor);

    Color getBackgroundColor();

    boolean hasBackgroundColor();

    void setPadding(float padding);

    float getPadding();

    void setRowSpan(int rowSpan);

    int getRowSpan();

    void setColSpan(int colSpan);

    int getColSpan();

    DocComponent getComponent();

    void setComponent(DocComponent component);
}
