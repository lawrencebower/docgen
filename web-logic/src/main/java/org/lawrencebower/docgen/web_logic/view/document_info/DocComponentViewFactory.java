package org.lawrencebower.docgen.web_logic.view.document_info;

import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.document_info.component.CheckBoxComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TableComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TextAreaComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TextComponentView;

public abstract class DocComponentViewFactory {

    public TableComponentView createTableComponentView(TableComponent tableComponent) {
        TableComponentView view = getTableComponentView();
        view.setComponent(tableComponent);
        return view;
    }

    public TextComponentView createTextComponentView(TextComponent textComponent) {
        TextComponentView view = getTextComponentView();
        view.setComponent(textComponent);
        return view;
    }

    public TextAreaComponentView createTextAreaComponentView(TableTextComponent textComponent) {
        TextAreaComponentView view = getTextAreaComponentView();
        view.setComponent(textComponent);
        return view;
    }

    public CheckBoxComponentView createCheckBoxComponentView(CheckBoxComponent checkBoxComponent) {
        CheckBoxComponentView view = getCheckBoxComponentView();
        view.setComponent(checkBoxComponent);
        return view;
    }

    protected abstract CheckBoxComponentView getCheckBoxComponentView();

    protected abstract TableComponentView getTableComponentView();

    protected abstract TextAreaComponentView getTextAreaComponentView();

    protected abstract TextComponentView getTextComponentView();

    public DocComponentView createComponentView(DocComponent component) {
        switch (component.getComponentType()) {
            case TEXT:
                return createTextComponentView((TextComponent) component);
            case TABLE_TEXT:
                return createTextComponentView((TableTextComponent) component);
            case TABLE:
                return createTableComponentView((TableComponent) component);
            case CHECKBOX:
                return createCheckBoxComponentView((CheckBoxComponent) component);
        }
        throw new DocGenException("DocComponent not mapped to ComponentView? " + component.getClass());
    }
}
