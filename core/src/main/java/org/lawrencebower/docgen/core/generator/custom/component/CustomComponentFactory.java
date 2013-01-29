package org.lawrencebower.docgen.core.generator.custom.component;

import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableComponent;
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

        CustomComponent returnComponent;

        switch (component.getComponentType()) {
            case TEXT:
                returnComponent = createCustomText((TextComponent) component);
                break;
            case LAYOUT_TABLE:
                returnComponent = createCustomTable((LayoutTableComponent) component);
                break;
            case VIEW_TABLE:
                returnComponent = createCustomTable((ViewTableComponent) component);
                break;
            case IMAGE:
                returnComponent = createCustomImage((ImageComponent) component);
                break;
            case LINE:
                returnComponent = createCustomLine((LineComponent) component);
                break;
            case NEWLINE:
                returnComponent = createCustomNewLine((NewLineComponent) component);
                break;
            default:
                Class<? extends DocComponent> componentClass = component.getClass();
                throw new DocGenException("DocComponent not mapped to CustomComponent? " + componentClass);
        }

        return returnComponent;
    }

    public CustomImageComponent createCustomImage(ImageComponent component) {

        ITextImageComponent iTextComponent = iTextFactory.getImageComponent();
        iTextComponent.setComponent(component);

        CustomImageComponent customComponent = getImageComponent();
        customComponent.setComponent(iTextComponent);

        return customComponent;
    }

    public CustomTableComponent createCustomTable(TableComponent<? extends TableRow,? extends TableHeaderRow> component) {

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
