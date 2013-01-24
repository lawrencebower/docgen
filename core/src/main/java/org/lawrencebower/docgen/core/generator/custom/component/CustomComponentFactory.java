package org.lawrencebower.docgen.core.generator.custom.component;

import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.*;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class CustomComponentFactory {

    @Autowired
    private ITextComponentFactory iTextFactory;

    public abstract CustomImageComponent getImageComponent();

    public abstract CustomTableComponent getTableComponent();

    public abstract CustomTextComponent getTextComponent();

    public abstract CustomLineComponent getLineComponent();

    public abstract CustomNewLineComponent getNewLineComponent();

    public CustomComponent createCustomComponent(DocComponent component) {
        switch (component.getComponentType()) {
            case TEXT:
                return createCustomText((TextComponent) component);
            case LAYOUT_TABLE:
                return createCustomTable((TableComponent) component);
            case VIEW_TABLE:
                return createCustomTable((TableComponent) component);
            case IMAGE:
                return createCustomImage((ImageComponent) component);
            case LINE:
                return createCustomLine((LineComponent) component);
            case NEWLINE:
                return createCustomNewLine((NewLineComponent) component);
        }
        throw new DocGenException("DocComponent not mapped to CustomComponent? " + component.getClass());
    }

    public CustomImageComponent createCustomImage(ImageComponent component) {

        ITextImageComponent iTextComponent = iTextFactory.getImageComponent();
        iTextComponent.setComponent(component);

        CustomImageComponent customComponent = getImageComponent();
        customComponent.setComponent(iTextComponent);

        return customComponent;
    }

    public CustomTableComponent createCustomTable(TableComponent component) {

        ITextTableComponent iTextComponent = iTextFactory.getTableComponent();
        iTextComponent.setComponent(component);

        CustomTableComponent customComponent = getTableComponent();
        customComponent.setComponent(iTextComponent);

        return customComponent;
    }

    public CustomTextComponent createCustomText(TextComponent component) {

        ITextTextComponent iTextComponent = iTextFactory.getTextComponent();
        iTextComponent.setComponent(component);

        CustomTextComponent customComponent = getTextComponent();
        customComponent.setComponent(iTextComponent);

        return customComponent;
    }

    public CustomNewLineComponent createCustomNewLine(NewLineComponent component) {

        ITextNewLineComponent iTextComponent = iTextFactory.getNewLineComponent();
        iTextComponent.setComponent(component);

        CustomNewLineComponent customComponent = getNewLineComponent();
        customComponent.setComponent(iTextComponent);

        return customComponent;
    }

    public CustomLineComponent createCustomLine(LineComponent component) {

        ITextLineComponent iTextComponent = iTextFactory.getLineComponent();
        iTextComponent.setComponent(component);

        CustomLineComponent customComponent = getLineComponent();
        customComponent.setComponent(iTextComponent);

        return customComponent;
    }
}
