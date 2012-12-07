package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public interface ITextComponent<T extends Element> {

    T createITextComponent();

    DocCoordinates getCoordinates();

    boolean isRenderBorder();

    HorizontalAlignment getAlignment();

    String getName();
}
