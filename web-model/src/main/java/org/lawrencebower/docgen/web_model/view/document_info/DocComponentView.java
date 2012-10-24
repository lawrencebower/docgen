package org.lawrencebower.docgen.web_model.view.document_info;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;

public class DocComponentView {

    public enum ComponentViewType {
        TEXT,
        TEXT_AREA,
        CHECK_BOX,
        CV
    }

    private DocComponent docComponent;
    private ComponentViewType componentViewType;
    private AutoMappedField autoMappedField;

    public DocComponentView(DocComponent docComponent, ComponentViewType type) {
        if (docComponent == null) {
            throw new DocGenException("DocComponent is null");
        }

        this.docComponent = docComponent;
        this.componentViewType = type;
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

        if (isTextComponent() || isTextAreaComponent()) {
            return ((TextComponent) docComponent).getTextString();
        }

        return value;
    }

    public boolean isTextComponent() {
        return componentViewType == ComponentViewType.TEXT;
    }

    public boolean isTextAreaComponent() {
        return componentViewType == ComponentViewType.TEXT_AREA;
    }

    public void setComponentValue(String value) {
        if (isTextComponent() || isTextAreaComponent()) {
            ((TextComponent) getDocComponent()).setText(value);
        }else{
            throw new DocGenException("Can not set the value for component of type " + docComponent.getClass());
        }
    }

    public boolean isAutoMappedField(){
        return autoMappedField != null;
    }

    public void setAutoMappedField(AutoMappedField autoMappedField) {
        this.autoMappedField = autoMappedField;
    }

    public AutoMappedField getAutoMappedField() {
        return autoMappedField;
    }
}
