package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public abstract class AbstractITextComponent
        <T extends Element, T2 extends DocComponent>
        implements ITextComponent<T> {

    protected T2 component;

    @Override
    public DocCoordinates getCoordinates() {
        return component.getCoordinates();
    }

    @Override
    public boolean isRenderBorder() {
        return component.isRenderBorder();
    }

    @Override
    public HorizontalAlignment getAlignment() {
        return component.getAlignment();
    }

    @Override
    public String getName() {
        return component.getName();
    }

    public void setComponent(T2 component) {
        this.component = component;
    }
}
