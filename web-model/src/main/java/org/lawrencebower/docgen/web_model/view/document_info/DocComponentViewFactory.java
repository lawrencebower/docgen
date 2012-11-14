package org.lawrencebower.docgen.web_model.view.document_info;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.web_model.view.document_info.component.CheckBoxComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.component.TableComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.component.TextAreaComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.component.TextComponentView;

public class DocComponentViewFactory {

    public TableComponentView createTextComponentView(TableComponent tableComponent) {
        return new TableComponentView(tableComponent);
    }

    public TextComponentView createTextComponentView(TextComponent textComponent) {
        return new TextComponentView(textComponent);
    }

    public TextComponentView createTextComponentView(TableTextComponent textComponent) {
        return new TextComponentView(textComponent);
    }

    public TextAreaComponentView createTextAreaComponentView(TableTextComponent textComponent) {
        return new TextAreaComponentView(textComponent);
    }

    public CheckBoxComponentView createCheckBoxComponentView(CheckBoxComponent checkBoxComponent) {
        return new CheckBoxComponentView(checkBoxComponent);
    }
}
