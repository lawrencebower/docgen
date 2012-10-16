package org.lawrencebower.docgen.web_model.view.document_info;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;

public class DocComponentView {

    private boolean editable;
    private DocComponent docComponent;

    public DocComponentView(DocComponent docComponent) {
        if (docComponent == null) {
            throw new DocGenException("DocComponent is null");
        }
        this.docComponent = docComponent;
    }

    public boolean isEditable() {
        return editable;
    }

    public void setEditable(boolean editable) {
        this.editable = editable;
    }

    public DocComponent getDocComponent() {
        return docComponent;
    }

    public String getName() {

        String name = "not set";

        if(docComponent.getName() != null){
            name = docComponent.getName();
        }

        return name;
    }

    public String getValue() {

        String value = "not set";

        if (docComponent instanceof TextComponent) {
            return ((TextComponent) docComponent).getTextString();
        }

        return value;
    }

    public boolean isIsTextComponent() {
        return docComponent.getComponentType() == DocComponentType.TEXT;
    }
}
