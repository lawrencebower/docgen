package org.lawrencebower.docgen.core.document.component;

import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public interface DocComponent {

    String getName();

    void setName(String name);

    HorizontalAlignment getAlignment();

    void setAlignment(HorizontalAlignment alignment);

    DocCoordinates getCoordinates();

    void setCoordinates(DocCoordinates coordinates);

    DocComponentType getComponentType();

    void setRenderBorder(boolean renderBorder);

    boolean isRenderBorder();
}
