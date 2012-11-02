package org.lawrencebower.docgen.core.generator.model.itext_component;

import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;

public abstract class ITextComponentFactory {
    public abstract ITextCheckBoxComponent getCheckBoxComponent();

    public abstract ITextImageComponent getImageComponent();

    public abstract ITextLineComponent getLineComponent();

    public abstract ITextNewLineComponent getNewLineComponent();

    public abstract ITextTableComponent getTableComponent();

    public abstract ITextTableTextComponent getTableTextComponent();

    public abstract ITextTextComponent getTextComponent();

    public ITextComponent createComponent(DocComponent component) {
        switch (component.getComponentType()) {
            case TEXT:
                return createTextComponent((TextComponent) component);
            case TABLE:
                return createTableComponent((TableComponent) component);
            case TABLE_TEXT:
                return createTableTextComponent((TableTextComponent) component);
            case IMAGE:
                return createImageComponent((ImageComponent) component);
            case LINE:
                return createLineComponent((LineComponent) component);
            case NEWLINE:
                return createNewLineComponent((NewLineComponent) component);
            case CHECKBOX:
                return createCheckBoxComponent((CheckBoxComponent) component);
        }
        throw new DocGenException("DocComponent not mapped to ITextComponent? " + component.getClass());
    }

    public ITextComponent createTableTextComponent(TableTextComponent component) {
        ITextTableTextComponent iTextComponent = getTableTextComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }

    public ITextComponent createCheckBoxComponent(CheckBoxComponent component) {
        ITextCheckBoxComponent iTextComponent = getCheckBoxComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }

    public ITextNewLineComponent createNewLineComponent(NewLineComponent component) {
        ITextNewLineComponent iTextComponent = getNewLineComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }

    public ITextLineComponent createLineComponent(LineComponent component) {
        ITextLineComponent iTextComponent = getLineComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }

    public ITextImageComponent createImageComponent(ImageComponent component) {
        ITextImageComponent iTextComponent = getImageComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }

    public ITextTableComponent createTableComponent(TableComponent component) {
        ITextTableComponent iTextComponent = getTableComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }

    public ITextTextComponent createTextComponent(TextComponent component) {
        ITextTextComponent iTextComponent = getTextComponent();
        iTextComponent.setComponent(component);
        return iTextComponent;
    }
}
