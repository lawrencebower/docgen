package org.lawrencebower.docgen.core.generator.overlay.component;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.*;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class OverlayComponentFactory {

    @Autowired
    private ITextComponentFactory iTextFactory;

    public abstract OverlayCheckBoxComponent getCheckBoxComponent();

    public abstract OverlayImageComponent getImageComponent();

    public abstract OverlayTableComponent getTableComponent();

    public abstract OverlayTextComponent getTextComponent();

    public OverlayComponent createOverlayComponent(DocComponent component) {
        switch (component.getComponentType()) {
            case TEXT:
                return createOverlayText((TextComponent) component);
            case LAYOUT_TABLE:
                return createOverlayTable((TableComponent) component);
            case IMAGE:
                return createOverlayImage((ImageComponent) component);
            case CHECKBOX:
                return createOverlayCheckBox((CheckBoxComponent) component);
        }
        throw new DocGenException("DocComponent not mapped to OverlayComponent? " + component.getClass());
    }

    public OverlayCheckBoxComponent createOverlayCheckBox(CheckBoxComponent component) {

        ITextCheckBoxComponent iTextComponent = iTextFactory.getCheckBoxComponent();
        iTextComponent.setComponent(component);

        OverlayCheckBoxComponent overlayComponent = getCheckBoxComponent();
        overlayComponent.setComponent(iTextComponent);

        return overlayComponent;
    }

    public OverlayImageComponent createOverlayImage(ImageComponent component) {

        ITextImageComponent iTextComponent = iTextFactory.getImageComponent();
        iTextComponent.setComponent(component);

        OverlayImageComponent overlayComponent = getImageComponent();
        overlayComponent.setComponent(iTextComponent);

        return overlayComponent;
    }

    public OverlayTableComponent createOverlayTable(TableComponent component) {

        ITextTableComponent iTextComponent = iTextFactory.getTableComponent();
        iTextComponent.setComponent(component);

        OverlayTableComponent overlayComponent = getTableComponent();
        overlayComponent.setComponent(iTextComponent);

        return overlayComponent;
    }

    public OverlayTextComponent createOverlayText(TextComponent component) {

        ITextTextComponent iTextComponent = iTextFactory.getTextComponent();
        iTextComponent.setComponent(component);

        OverlayTextComponent overlayComponent = getTextComponent();
        overlayComponent.setComponent(iTextComponent);

        return overlayComponent;
    }

}
