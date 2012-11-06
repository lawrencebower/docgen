package org.lawrencebower.docgen.web_model.view.document_info;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;

public abstract class DocComponentView<T extends DocComponent> {

    public enum ComponentViewType {
        TEXT,
        TEXT_AREA,
        CHECK_BOX,
        TABLE,
        CV
    }

    protected T docComponent;
    protected ComponentViewType componentViewType;
    private AutoMappedField autoMappedField;

    public DocComponentView(T docComponent) {

        if (docComponent == null) {
            throw new DocGenException("DocComponent is null");
        }

        this.docComponent = docComponent;
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

    public abstract String getComponentValue();

    public abstract void setComponentFromParamString(String value);

    public ComponentViewType getComponentViewType(){
        return componentViewType;
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

    public boolean isText(){
        return componentViewType == ComponentViewType.TEXT;
    }

    public boolean isTextArea(){
        return componentViewType == ComponentViewType.TEXT_AREA;
    }

    @Override
    public boolean equals(Object obj) {

        if(!(obj instanceof DocComponentView)){
            return false;
        }

        DocComponentView compareTo = (DocComponentView) obj;
        EqualsBuilder builder = new EqualsBuilder();
        builder.append(this.getName(), compareTo.getName());

        return builder.isEquals();
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(this.getName());
        return builder.toHashCode();
    }
}
